// helpers .. merge and download binary file.
  function m(a,b) { for (var i in b) a[i]=b[i]; return a; }
  function fileAsArray(name,complete,prog) {
    var r=m(new XMLHttpRequest(),{responseType:'arraybuffer',onprogress:prog,onload:function(){complete&&complete(this.response)}});
    r.open("GET",name,true); r.send();
  }
  
// helpers .. 
  function rX(m, angle) {
    var s=Math.sin(angle),c=Math.cos(angle),a10=m[4],a11=m[5],a12=m[6],a13=m[7],a20=m[8],a21=m[9],a22=m[10],a23=m[11];
    m[4]=a10*c+a20*s;m[5]=a11*c+a21*s;m[6]=a12*c+a22*s;m[7]=a13*c+a23*s;
    m[8]=a10*-s+a20*c;m[9]=a11*-s+a21*c;m[10]=a12*-s+a22*c;m[11]=a13*-s+a23*c;
    return m;
  };
  function rY(m, angle) {
    var s=Math.sin(angle),c=Math.cos(angle),a00=m[0],a01=m[1],a02=m[2],a03=m[3],a20=m[8],a21=m[9],a22=m[10],a23=m[11];
    m[0]=a00*c+a20*-s;m[1]=a01*c+a21*-s;m[2]=a02*c+a22*-s;m[3]=a03*c+a23*-s;
    m[8]=a00*s+a20*c;m[9]=a01*s+a21*c;m[10]=a02*s+a22*c;m[11]=a03*s+a23*c;
    return m;
  };
  function t(m,v) {
    var x=v[0],y=v[1],z=v[2];
    m[12]=m[0]*x+m[4]*y+m[8]*z+m[12];
    m[13]=m[1]*x+m[5]*y+m[9]*z+m[13];
    m[14]=m[2]*x+m[6]*y+m[10]*z+m[14];
    m[15]=m[3]*x+m[7]*y+m[11]*z+m[15];
    return m;
  }
  function i() { return new Float32Array([1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]) };
  
// png compressed raw file support..
  function png_to_raw(png) {
    var c = document.createElement('canvas'), x=c.width=png.width, y=c.height=png.height, gl=c.getContext('webgl');

    var texture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.texImage2D( gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, png);
     
    fb = gl.createFramebuffer();
    gl.bindFramebuffer(gl.FRAMEBUFFER, fb);
    gl.framebufferTexture2D( gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, texture, 0);

    var res = new Uint8Array(x*y*4);
    gl.readPixels(0,0,x,y,gl.RGBA,gl.UNSIGNED_BYTE,res);

    gl.deleteTexture(texture);
    gl.deleteFramebuffer(fb);
    return res.buffer;
  }

// Download polygon and accelerator data. (they're called .html to fool github pages into compressing them ..)  
  var todo=2,map,polyData;

  var mapI = new Image(), polyDataI = new Image();
  mapI.onload = function() { map = new Float32Array(png_to_raw(this)); if(!--todo) allthere(); };
  polyDataI.onload = function() { polyData = new Float32Array(png_to_raw(this)); if (!--todo) allthere()};
  mapI.src = 'map2.bin.png';
  polyDataI.src = 'polydata2.bin.png';  

// All data is there .. compile shaders and render ..   
  function allthere(){

  // settings.
    var gridres=128,totX=4096,totY=391,pbrtShowrender=true,pbrtBounces=2,pbrtBatch=1,pbrtSamples=Math.floor((parseInt(document.location.hash.slice(1))||200)/pbrtBatch);
    var pbrtGrid = { bbox : new Float32Array([-16.35320053100586,-3.3039399147033692,-13.719999885559082,31.68820018768311,13.706639957427978,24.6798002243042])};

  // Shader helpers.
    function createShader(gl, source, type) { var shader=gl.createShader(type); gl.shaderSource(shader, source); gl.compileShader(shader); return shader; }
    function createProgram(gl, vertexShaderSource, fragmentShaderSource) {
        var program = gl.createProgram();
        gl.attachShader(program, createShader(gl, vertexShaderSource, gl.VERTEX_SHADER)); 
        gl.attachShader(program, createShader(gl, fragmentShaderSource, gl.FRAGMENT_SHADER));
        gl.linkProgram(program);
        return program;
    };

  // Offscreen float buffers.
    function createOffscreen(gl,width,height) {
       var colorTexture = gl.createTexture();
       gl.bindTexture(gl.TEXTURE_2D, colorTexture);
         gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, width, height, 0, gl.RGBA, gl.FLOAT, null);
         gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
         gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
       gl.bindTexture(gl.TEXTURE_2D, null);

       var depthBuffer = gl.createRenderbuffer();
       gl.bindRenderbuffer(gl.RENDERBUFFER, depthBuffer);
         gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, width, height);
       gl.bindRenderbuffer(gl.RENDERBUFFER, null);

       var framebuffer = gl.createFramebuffer();
       gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
         gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, colorTexture, 0);
         gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, depthBuffer);
       gl.bindFramebuffer(gl.FRAMEBUFFER, null);

       return {
         framebuffer:framebuffer,colorTexture:colorTexture,depthBuffer:depthBuffer,width:width,height:height,gl:gl,
         bind   : function() { this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, this.framebuffer); this.gl.viewport(0,0,this.width,this.height); },
         unbind : function() { this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, null); },
         delete : function() { this.gl.deleteRenderbuffer(this.depthBuffer); this.gl.deleteFramebuffer(this.framebuffer); this.gl.deleteTexture(this.colorTexture); }
       }
    }

  // setup output canvas, gl context and offscreen.
    var canvas = m(document.body.appendChild(document.createElement('canvas')),{width:800,height:450});
    m(canvas.style,{width:'800px',height:'450px'});
    var gl = canvas.getContext('webgl2'); if (!gl) alert('webGL2 required !');
    var ext1 = gl.getExtension('EXT_color_buffer_float'); if (!ext1) alert('videocard required');
    var ofscreen1 = createOffscreen(gl,canvas.width,canvas.height);
  
  // Shaders for accumulation and tonemap.
    var vs2=`#version 300 es
      #define POSITION_LOCATION 0
      precision lowp float;
      layout(location = POSITION_LOCATION) in vec3 position;
      void main() { gl_Position = vec4(position, 1.0); }`;

    var fs2=`#version 300 es
      precision lowp float;
      precision lowp sampler2D;
      uniform sampler2D accum;
      uniform float count;
      out vec4 color;
      void main() { color = vec4(sqrt(texelFetch(accum,ivec2(gl_FragCoord.xy),0).rgb*count),1.0); }
    `;

  // Actual Path tracing shaders.
    var vs=`#version 300 es
      #define POSITION_LOCATION 0
      precision highp float;
      precision highp int;
      uniform mat4 MVP;
      layout(location = POSITION_LOCATION) in vec3 position;
      out vec3 origin;
      void main()
      {
          origin = (MVP * vec4(0.0,0.0,0.0,1.0)).xyz;
          gl_Position = vec4(position, 1.0);
      }`;

    var fs=`#version 300 es
      precision highp float;
      precision highp int;
      precision highp sampler3D;
      precision highp sampler2D;

      #define EPSILON 0.000001

      uniform vec2 resolution;
      uniform float inseed;
      uniform int incount;
      
      uniform mat4 MVP, proj;

      uniform sampler3D grid;
      uniform sampler2D tris;
      uniform vec3 bbina, bbinb;

      vec3 bboxA, bboxB;

      in vec3 origin;
      out vec4 color;
      
      uint N = ${pbrtSamples}u, i;

      float seed;
      float minc(const vec3 x) { return min(x.x,min(x.y,x.z)); }
      
      float random_ofs=0.0;
      vec3 cosWeightedRandomHemisphereDirectionHammersley( const vec3 n ) {
        float x = float(i)/float(N); 
        i = (i << 16u) | (i >> 16u);
        i = ((i & 0x55555555u) << 1u) | ((i & 0xAAAAAAAAu) >> 1u);
        i = ((i & 0x33333333u) << 2u) | ((i & 0xCCCCCCCCu) >> 2u);
        i = ((i & 0x0F0F0F0Fu) << 4u) | ((i & 0xF0F0F0F0u) >> 4u);
        i = ((i & 0x00FF00FFu) << 8u) | ((i & 0xFF00FF00u) >> 8u);
        vec2  r = vec2(x,(float(i) * 2.32830643653086963e-10 * 6.2831) + random_ofs);
        vec3  uu=normalize(cross(n, vec3(1.0,1.0,0.0))), vv=cross( uu, n );
        float sqrtx = sqrt(r.x);
        return normalize(vec3( sqrtx*cos(r.y)*uu + sqrtx*sin(r.y)*vv + sqrt(1.0-r.x)*n ));
      }

      vec4 trace( inout vec3 realori, const vec3 dir) {
        float len=0.0,l,b,mint=1000.0;
        vec2 minuv, mintri, cpos;
        vec3 scaler=vec3(bbinb/${gridres}.0)/dir,orig=realori,v0,v1,v2;
        for (int i=0;i<150;i++){
           vec3 txc=(orig-bboxA)*bboxB;
           if ( txc != clamp(txc,0.0,1.0)) break;
           vec3 tex=textureLod(grid,txc,0.0).rgb;
           for(int tri=0; tri<512; tri++) { 
              if (tex.b<=0.0) break; cpos=tex.rg; tex.rb+=vec2(3.0/4096.0,-1.0); 
              v1 = textureLodOffset(tris,cpos,0.0,ivec2(1,0)).rgb;
              v2 = textureLodOffset(tris,cpos,0.0,ivec2(2,0)).rgb;
              vec3 P = cross(dir,v2); float det=dot(v1,P); if (det>-EPSILON) continue;
              v0 = textureLod(tris,cpos,0.0).rgb;
              vec3 T=realori-v0; float invdet=1.0/det; float u=dot(T,P)*invdet; if (u < 0.0 || u > 1.0) continue;
              vec3 Q=cross(T,v1); float v=dot(dir,Q)*invdet; if(v<0.0||u+v>1.0) continue;
              float t=dot(v2, Q)*invdet; if (t>EPSILON  && t<mint) { mint=t; mintri=cpos; minuv=vec2(u,v); }  
           }
           b=max(0.0,-tex.b-1.0); txc=fract(txc*${gridres}.0);
           l=minc(scaler*mix(b+1.0-txc,-b-txc,vec3(lessThan(dir,vec3(0.0)))))+EPSILON*50.0;
           len += l;
           if (mint <= len) {
             realori += dir*(mint);
             mintri += vec2(0.0,1.0/4.0);
             vec3 n0 =  -textureLod(tris,mintri,0.0).rgb;
             vec3 n1 =  -textureLodOffset(tris,mintri,0.0,ivec2(1,0)).rgb;
             vec3 n2 =  -textureLodOffset(tris,mintri,0.0,ivec2(2,0)).rgb;
             return vec4(normalize(n0*(1.0-minuv.x-minuv.y) + n1*minuv.x + n2*minuv.y),mint); 
           }  
           orig += dir*l;
        }
        return vec4(0.0);  
      }
      
      void main()
      {
          bboxA=bbina; bboxB=1.0/bbinb; i=uint(incount);
          vec2 fc = vec2(gl_FragCoord.xy), fcu=fc/resolution;
          seed = inseed +fcu.x+fcu.y; 
          vec2 aa = fract(sin(vec2(seed,seed+0.1))*vec2(43758.5453123,22578.1459123));
          random_ofs = fract(gl_FragCoord.x * gl_FragCoord.y * inseed + aa.x)*6.2831;
          vec4 view = proj * vec4((fc+aa)/(resolution/2.0)-1.0,0.0,1.0);
          view = normalize(MVP*vec4(view.xyz/view.w,0.0));
          vec3 orig=origin,v1=(bboxA-orig)/view.xyz,v2=v1+(bbinb-vec3(0.2))/view.xyz,far=max(v1,v2),near=min(v1,v2);
          float en=max(near.x,max(near.y,near.z)), ex=min(far.x,min(far.y,far.z));
          if (ex < 0.0 || en > ex) { color=vec4(1.0); return; }
          orig += max(0.0,en)*view.xyz;
          vec4 hit=trace(orig,view.xyz);
          if (hit.w <= 0.0) { color.rgb = vec3(1.0); return; }
          hit=trace(orig, -cosWeightedRandomHemisphereDirectionHammersley(hit.xyz));
          if (hit.w <= 0.0) { color.rgb = vec3(0.8); return; }
      }`;

  // Upload polygon and acceleration data.
    var texture = gl.createTexture();
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_3D, texture);
    gl.texParameteri(gl.TEXTURE_3D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_3D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texImage3D( gl.TEXTURE_3D, 0, gl.RGB32F, gridres, gridres, gridres, 0, gl.RGB, gl.FLOAT, map );  

    var texture2 = gl.createTexture();
    gl.activeTexture(gl.TEXTURE1);
    gl.bindTexture(gl.TEXTURE_2D, texture2);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texImage2D( gl.TEXTURE_2D, 0, gl.RGB32F, totX, totY*4, 0, gl.RGB, gl.FLOAT, polyData );  

  // Create the path tracing program, grab the uniforms.
    var program = createProgram(gl, vs, fs);
    var mvpLocation         = gl.getUniformLocation(program, 'MVP');
    var pLocation           = gl.getUniformLocation(program, 'proj');
    var uniformgridLocation = gl.getUniformLocation(program, 'grid');
    var uniformtrisLocation = gl.getUniformLocation(program, 'tris');
    var uniformSeed         = gl.getUniformLocation(program, 'inseed');
    var uniformCount        = gl.getUniformLocation(program, 'incount');
    var uniformbbaLocation  = gl.getUniformLocation(program, 'bbina');
    var uniformbbbLocation  = gl.getUniformLocation(program, 'bbinb');
    var uniformresLocation  = gl.getUniformLocation(program, 'resolution');

  // Create the accumulation program, grab thos uniforms.
    var program2 = createProgram(gl, vs2, fs2);
    var uniformAccumLocation = gl.getUniformLocation(program2, 'accum');
    var uniformCountLocation = gl.getUniformLocation(program2, 'count');

  // Setup the quad that will drive the rendering.
    var vertexPosBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, vertexPosBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 0, 1, -1, 0, 1, 1, 0, 1, 1, 0, -1, 1, 0, -1, -1, 0]), gl.STATIC_DRAW);
    gl.bindBuffer(gl.ARRAY_BUFFER, null);      

    var vertexArray = gl.createVertexArray();
    gl.bindVertexArray(vertexArray);
      var vertexPosLocation = 0;
      gl.enableVertexAttribArray(vertexPosLocation);
      gl.bindBuffer(gl.ARRAY_BUFFER, vertexPosBuffer);
      gl.vertexAttribPointer(vertexPosLocation, 3, gl.FLOAT, false, 0, 0);
      gl.bindBuffer(gl.ARRAY_BUFFER, null);
    gl.bindVertexArray(null);

  // Setup some matrices (stole values from jimmy|rig)
    var matrix     = new Float32Array([6.123234262925839e-17, 0, 1, 0, -0.8660253882408142, 0.5, 5.302876566937394e-17, 0, -0.5, -0.8660253882408142, 3.0616171314629196e-17, 0, 6.535898208618164, 19.320507049560547, -4.0020835038019837e-16, 1]);
    var matrix2    = new Float32Array([1.7777777910232544,0,0,0,0,1,0,0,0,0,0,-0.49950000643730164,0,0,1,0.5005000233650208]);
    var viewportMV = new Float32Array(matrix); 
    var accum_count=1, diff=true, abort=false;

  // frame handler.    
    function frame() {
    // Do we need to restart rendering (i.e. viewport change)
      if (diff) {
        matrix.set(viewportMV);
        ofscreen1.bind(); gl.clear(gl.COLOR_BUFFER_BIT); ofscreen1.unbind();
        accum_count=1;
        abort=undefined;
        diff=false;
      }

    // Render more samples.
      if (!abort) {  
      // Bind the offscreen and render a new sample.
        ofscreen1.bind();
          gl.useProgram(program);
          gl.uniformMatrix4fv(mvpLocation, false, matrix);
          gl.uniformMatrix4fv(pLocation, false, matrix2);
          gl.uniform1i(uniformgridLocation, 0);
          gl.uniform1i(uniformtrisLocation, 1);
          gl.uniform3fv(uniformbbaLocation, pbrtGrid.bbox.slice(0,3));
          gl.uniform3fv(uniformbbbLocation, pbrtGrid.bbox.slice(3,6));
          gl.uniform2fv(uniformresLocation, new Float32Array([canvas.width,canvas.height]));
          gl.uniform1f(uniformSeed,Math.random());
          gl.uniform1i(uniformCount,(accum_count)%pbrtSamples);

          gl.activeTexture(gl.TEXTURE0);
          gl.bindTexture(gl.TEXTURE_3D, texture);
          gl.activeTexture(gl.TEXTURE1);
          gl.bindTexture(gl.TEXTURE_2D, texture2);

          gl.enable(gl.BLEND);
          gl.blendFunc(gl.ONE,gl.ONE);
          gl.bindVertexArray(vertexArray);
            gl.drawArrays(gl.TRIANGLES, 0, 6);
          gl.bindVertexArray(null);
          gl.disable(gl.BLEND);
        ofscreen1.unbind();

      // Display progress (mixdown from float to ldr)  
        gl.useProgram(program2);
        gl.uniform1i(uniformAccumLocation, 0);
        gl.uniform1f(uniformCountLocation, 1.0/accum_count);
        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, ofscreen1.colorTexture);

        gl.bindVertexArray(vertexArray);
          gl.drawArrays(gl.TRIANGLES, 0, 6);
        gl.bindVertexArray(null);

        gl.bindTexture(gl.TEXTURE_2D, null);

      // Stop if we're done.
        if (++accum_count>pbrtSamples) abort =true;
      }
      requestAnimationFrame(frame);
    }
    requestAnimationFrame(frame);

    var angle=-Math.PI/2, angle2=Math.PI/3,zoom=0;
    canvas.oncontextmenu =function(e) { e.preventDefault(); e.stopPropagation(); }
    canvas.onmousemove = function(e) {
      if (!e.buttons) return;
      if (e.buttons==1) { angle += e.movementX/100; angle2 += e.movementY/100; } else { zoom += e.movementX/10; }
      viewportMV = t(rX(rY(i(),angle),angle2),[0,4,-20+zoom]);
      diff = true;
    }
  };
