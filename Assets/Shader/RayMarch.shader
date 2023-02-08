// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "Hidden/RayMarch"
{
    Properties
    {
        _MainTex ("Texture", 2D) = "white" {}
    }
    SubShader
    {
        // No culling or depth
        Cull Off ZWrite Off ZTest Always

        Pass
        {
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag

            #include "UnityCG.cginc"

            // Provided by our script
            uniform float4x4 _FrustumCornersES;
            uniform sampler2D _MainTex;
            uniform float4 _MainTex_TexelSize;
            uniform float4x4 _CameraInvViewMatrix;
            uniform float3 _CameraWS;
            uniform float3 _LightDir;
            uniform sampler2D _CameraDepthTexture;
            uniform float4x4 _MatTorus_InvModel;
            
            // Input to vertex shader
            struct appdata
            {
                // Remember, the z value here contains the index of _FrustumCornersES to use
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
            };

            // Output of vertex shader / input to fragment shader
            struct v2f
            {
                float4 pos : SV_POSITION;
                float2 uv : TEXCOORD0;
                float3 ray : TEXCOORD1;
            };

            v2f vert (appdata v)
            {
                v2f o;
                
                // Index passed via custom blit function in RaymarchGeneric.cs
                half index = v.vertex.z;
                v.vertex.z = 0.1;
                
                o.pos = UnityObjectToClipPos(v.vertex);
                o.uv = v.uv.xy;
                
                #if UNITY_UV_STARTS_AT_TOP
                if (_MainTex_TexelSize.y < 0)
                    o.uv.y = 1 - o.uv.y;
                #endif

                // Get the eyespace view ray (normalized)
                o.ray = _FrustumCornersES[(int)index].xyz;

                o.ray /= abs(o.ray.z);
                
                // Transform the ray from eyespace to worldspace
                // Note: _CameraInvViewMatrix was provided by the script
                o.ray = mul(_CameraInvViewMatrix, o.ray);

                return o;
            }

            // Union
            // Adapted from: http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
            float opU( float d1, float d2 )
            {
                return min(d1,d2);
            }
            
            // Subtraction
            // Adapted from: http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
            float opS( float d1, float d2 )
            {
                return max(-d1,d2);
            }

            // Intersection
            // Adapted from: http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
            float opI( float d1, float d2 )
            {
                return max(d1,d2);
            }
            
            float opSmoothUnion( float d1, float d2, float k )
            {
                float h = clamp( 0.5 + 0.5*(d2-d1)/k, 0.0, 1.0 );
                return lerp( d2, d1, h ) - k*h*(1.0-h);
            }
            
            float sdSphere( float3 p, float3 o, float r )
            {
                return length(p - o) - r;
            }
            
            // Torus
            // t.x: diameter
            // t.y: thickness
            // Adapted from: http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
            float sdTorus(float3 p, float2 t)
            {
                float2 q = float2(length(p.xz) - t.x, p.y);
                return length(q) - t.y;
            }

            
            float mapUnion(float3 p) {
                return opSmoothUnion( sdSphere(p, float3(0, 0, 0), 0.5), sdSphere(p, float3(0, 0.6, 0), 0.25), 0.05);
            }
            
            // This is the distance field function.  The distance field represents the closest distance to the surface
            // of any object we put in the scene.  If the given point (point p) is inside of an object, we return a
            // negative answer.
            float map(float3 p) {

                return mapUnion(p);
                // float4 pp = mul(_MatTorus_InvModel, float4(p, 1));
                // return sdTorus(pp.xyz, float2(1, 0.2));//sdSphere(p, 0.5);//
            }


            float3 calcNormal(in float3 pos)
            {
                // epsilon - used to approximate dx when taking the derivative
                const float2 eps = float2(0.001, 0.0);

                // The idea here is to find the "gradient" of the distance field at pos
                // Remember, the distance field is not boolean - even if you are inside an object
                // the number is negative, so this calculation still works.
                // Essentially you are approximating the derivative of the distance field at this point.
                float3 nor = float3(
                    map(pos + eps.xyy).x - map(pos - eps.xyy).x,
                    map(pos + eps.yxy).x - map(pos - eps.yxy).x,
                    map(pos + eps.yyx).x - map(pos - eps.yyx).x);
                return normalize(nor);
            }
            
            // Raymarch along given ray
            // ro: ray origin
            // rd: ray direction
            fixed4 raymarch(float3 ro, float3 rd, float depth) {
                fixed4 ret = fixed4(0,0,0,0);

                const int maxstep = 64;
                float t = 0; // current distance traveled along ray
                for (int i = 0; i < maxstep; ++i) {
                    float3 p = ro + rd * t; // World space position of sample
                    float d = map(p);       // Sample of distance field (see map())

                    if(t > depth)
                    {
                        ret = 0;
                        break;
                    }
                    
                    // If the sample <= 0, we have hit something (see map()).
                    if (d < 0.001) {
                        // Simply return a gray color if we have hit an object
                        // We will deal with lighting later.
                        ret = fixed4( dot( calcNormal(p), 1 - _LightDir) * float3(0, 1, 0), 1);
                        break;
                    }

                    // If the sample > 0, we haven't hit anything yet so we should march forward
                    // We step forward by distance d, because d is the minimum distance possible to intersect
                    // an object (see map()).
                    t += d;
                }

                return ret;
            }

            fixed4 frag (v2f i) : SV_Target
            {
                // ray direction
                float3 rd = normalize(i.ray.xyz);
                // ray origin (camera position)
                float3 ro = _CameraWS;

                float depth = LinearEyeDepth(tex2D(_CameraDepthTexture, i.uv).r);
                depth = length( i.ray * depth );
                
                fixed3 col = tex2D(_MainTex,i.uv); // Color of the scene before this shader was run
                fixed4 add = raymarch(ro, rd, depth);

                // Returns final color using alpha blending
                return fixed4(col*(1.0 - add.w) + add.xyz * add.w,1.0);
            }
            ENDCG
        }
    }
}
