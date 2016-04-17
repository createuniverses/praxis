// --------------------------------------------------------------
//
//  This Shader generates a font-texture
//
// --------------------------------------------------------------
//
//  The ascii table is split into 32x8 characters each 8 pixels
//  wide and 12 pixels high. This gives a block of 256x96 pixels
//  starting at the top left (0, iResolution.y)
//
// --------------------------------------------------------------

//  Created by Bart Verheijen 2016
//  License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.




// font data was inspired by the work of Flyguy:
// https://www.shadertoy.com/view/Mt2GWD


#define CHAR_SIZE vec2(8, 12)

vec4 cond(bool b)
{
  if(b)
  {
    return vec4(0.0);
  }
  else
  {
    return vec4(1.0);
  }
}

vec4 asciiToSprite(float a)
{
    vec4 pixel = vec4(0);
    
    pixel = mix(pixel,vec4(0x000000, 0x000000, 0x000000, 0x000000), cond(a<0.5));
    pixel = mix(pixel, vec4(0x007ec3, 0x81a581, 0xbd99c3, 0x7e0000), cond(a<1.5));
    pixel = mix(pixel, vec4(0x007eff, 0xffdbff, 0xc3e7ff, 0x7e0000), cond(a<2.5));
    pixel = mix(pixel, vec4(0x000044, 0xeefefe, 0xfe7c38, 0x100000), cond(a<3.5));
    pixel = mix(pixel, vec4(0x001038, 0x7cfefe, 0x7c3810, 0x000000), cond(a<4.5));
    pixel = mix(pixel, vec4(0x00183c, 0x3cffe7, 0xe71818, 0x7e0000), cond(a<5.5));
    pixel = mix(pixel, vec4(0x00183c, 0x7effff, 0x7e1818, 0x7e0000), cond(a<6.5));
    pixel = mix(pixel, vec4(0x000000, 0x003c7e, 0x7e3c00, 0x000000), cond(a<7.5));
    pixel = mix(pixel, vec4(0xffffff, 0xffc381, 0x81c3ff, 0xffffff), cond(a<8.5));
    pixel = mix(pixel, vec4(0x00003c, 0x7e6642, 0x42667e, 0x3c0000), cond(a<9.5));
    pixel = mix(pixel, vec4(0xffffc3, 0x8199bd, 0xbd9981, 0xc3ffff), cond(a<10.5));
    pixel = mix(pixel, vec4(0x003e0e, 0x3a72f8, 0xcccccc, 0x780000), cond(a<11.5));
    pixel = mix(pixel, vec4(0x003c66, 0x66663c, 0x187e18, 0x180000), cond(a<12.5));
    pixel = mix(pixel, vec4(0x001f19, 0x191f18, 0x1878f8, 0x700000), cond(a<13.5));
    pixel = mix(pixel, vec4(0x007f63, 0x7f6363, 0x6367e7, 0xe6c000), cond(a<14.5));
    pixel = mix(pixel, vec4(0x000018, 0xdb7ee7, 0xe77edb, 0x180000), cond(a<15.5));
    pixel = mix(pixel, vec4(0x0080c0, 0xe0f8fe, 0xf8e0c0, 0x800000), cond(a<16.5));
    pixel = mix(pixel, vec4(0x000206, 0x0e3efe, 0x3e0e06, 0x020000), cond(a<17.5));
    pixel = mix(pixel, vec4(0x00183c, 0x7e1818, 0x187e3c, 0x180000), cond(a<18.5));
    pixel = mix(pixel, vec4(0x006666, 0x666666, 0x000066, 0x660000), cond(a<19.5));
    pixel = mix(pixel, vec4(0x007fdb, 0xdbdb7b, 0x1b1b1b, 0x1b0000), cond(a<20.5));
    pixel = mix(pixel, vec4(0x007e63, 0x303c66, 0x663c0c, 0xc67e00), cond(a<21.5));
    pixel = mix(pixel, vec4(0x000000, 0x000000, 0x00fefe, 0xfe0000), cond(a<22.5));
    pixel = mix(pixel, vec4(0x00183c, 0x7e1818, 0x187e3c, 0x187e00), cond(a<23.5));
    pixel = mix(pixel, vec4(0x00183c, 0x7e1818, 0x181818, 0x180000), cond(a<24.5));
    pixel = mix(pixel, vec4(0x001818, 0x181818, 0x187e3c, 0x180000), cond(a<25.5));
    pixel = mix(pixel, vec4(0x000000, 0x180cfe, 0x0c1800, 0x000000), cond(a<26.5));
    pixel = mix(pixel, vec4(0x000000, 0x3060fe, 0x603000, 0x000000), cond(a<27.5));
    pixel = mix(pixel, vec4(0x000000, 0x00c0c0, 0xc0fe00, 0x000000), cond(a<28.5));
    pixel = mix(pixel, vec4(0x000000, 0x2466ff, 0x662400, 0x000000), cond(a<29.5));
    pixel = mix(pixel, vec4(0x000010, 0x103838, 0x7c7cfe, 0xfe0000), cond(a<30.5));
    pixel = mix(pixel, vec4(0x0000fe, 0xfe7c7c, 0x383810, 0x100000), cond(a<31.5));
    pixel = mix(pixel, vec4(0x000000, 0x000000, 0x000000, 0x000000), cond(a<32.5));
    pixel = mix(pixel, vec4(0x003078, 0x787830, 0x300030, 0x300000), cond(a<33.5));
    pixel = mix(pixel, vec4(0x006666, 0x662400, 0x000000, 0x000000), cond(a<34.5));
    pixel = mix(pixel, vec4(0x006c6c, 0xfe6c6c, 0x6cfe6c, 0x6c0000), cond(a<35.5));
    pixel = mix(pixel, vec4(0x30307c, 0xc0c078, 0x0c0cf8, 0x303000), cond(a<36.5));
    pixel = mix(pixel, vec4(0x000000, 0xc4cc18, 0x3060cc, 0x8c0000), cond(a<37.5));
    pixel = mix(pixel, vec4(0x0070d8, 0xd870fa, 0xdeccdc, 0x760000), cond(a<38.5));
    pixel = mix(pixel, vec4(0x003030, 0x306000, 0x000000, 0x000000), cond(a<39.5));
    pixel = mix(pixel, vec4(0x000c18, 0x306060, 0x603018, 0x0c0000), cond(a<40.5));
    pixel = mix(pixel, vec4(0x006030, 0x180c0c, 0x0c1830, 0x600000), cond(a<41.5));
    pixel = mix(pixel, vec4(0x000000, 0x663cff, 0x3c6600, 0x000000), cond(a<42.5));
    pixel = mix(pixel, vec4(0x000000, 0x18187e, 0x181800, 0x000000), cond(a<43.5));
    pixel = mix(pixel, vec4(0x000000, 0x000000, 0x000038, 0x386000), cond(a<44.5));
    pixel = mix(pixel, vec4(0x000000, 0x0000fe, 0x000000, 0x000000), cond(a<45.5));
    pixel = mix(pixel, vec4(0x000000, 0x000000, 0x000038, 0x380000), cond(a<46.5));
    pixel = mix(pixel, vec4(0x000002, 0x060c18, 0x3060c0, 0x800000), cond(a<47.5));
    pixel = mix(pixel, vec4(0x007cc6, 0xceded6, 0xf6e6c6, 0x7c0000), cond(a<48.5));
    pixel = mix(pixel, vec4(0x001030, 0xf03030, 0x303030, 0xfc0000), cond(a<49.5));
    pixel = mix(pixel, vec4(0x0078cc, 0xcc0c18, 0x3060cc, 0xfc0000), cond(a<50.5));
    pixel = mix(pixel, vec4(0x0078cc, 0x0c0c38, 0x0c0ccc, 0x780000), cond(a<51.5));
    pixel = mix(pixel, vec4(0x000c1c, 0x3c6ccc, 0xfe0c0c, 0x1e0000), cond(a<52.5));
    pixel = mix(pixel, vec4(0x00fcc0, 0xc0c0f8, 0x0c0ccc, 0x780000), cond(a<53.5));
    pixel = mix(pixel, vec4(0x003860, 0xc0c0f8, 0xcccccc, 0x780000), cond(a<54.5));
    pixel = mix(pixel, vec4(0x00fec6, 0xc6060c, 0x183030, 0x300000), cond(a<55.5));
    pixel = mix(pixel, vec4(0x0078cc, 0xcccc78, 0xcccccc, 0x780000), cond(a<56.5));
    pixel = mix(pixel, vec4(0x0078cc, 0xcccc7c, 0x181830, 0x700000), cond(a<57.5));
    pixel = mix(pixel, vec4(0x000000, 0x383800, 0x003838, 0x000000), cond(a<58.5));
    pixel = mix(pixel, vec4(0x000000, 0x383800, 0x003838, 0x183000), cond(a<59.5));
    pixel = mix(pixel, vec4(0x000c18, 0x3060c0, 0x603018, 0x0c0000), cond(a<60.5));
    pixel = mix(pixel, vec4(0x000000, 0x007e00, 0x7e0000, 0x000000), cond(a<61.5));
    pixel = mix(pixel, vec4(0x006030, 0x180c06, 0x0c1830, 0x600000), cond(a<62.5));
    pixel = mix(pixel, vec4(0x0078cc, 0x0c1830, 0x300030, 0x300000), cond(a<63.5));
    pixel = mix(pixel, vec4(0x007cc6, 0xc6dede, 0xdec0c0, 0x7c0000), cond(a<64.5));
    pixel = mix(pixel, vec4(0x003078, 0xcccccc, 0xfccccc, 0xcc0000), cond(a<65.5));
    pixel = mix(pixel, vec4(0x00fc66, 0x66667c, 0x666666, 0xfc0000), cond(a<66.5));
    pixel = mix(pixel, vec4(0x003c66, 0xc6c0c0, 0xc0c666, 0x3c0000), cond(a<67.5));
    pixel = mix(pixel, vec4(0x00f86c, 0x666666, 0x66666c, 0xf80000), cond(a<68.5));
    pixel = mix(pixel, vec4(0x00fe62, 0x60647c, 0x646062, 0xfe0000), cond(a<69.5));
    pixel = mix(pixel, vec4(0x00fe66, 0x62647c, 0x646060, 0xf00000), cond(a<70.5));
    pixel = mix(pixel, vec4(0x003c66, 0xc6c0c0, 0xcec666, 0x3e0000), cond(a<71.5));
    pixel = mix(pixel, vec4(0x00cccc, 0xccccfc, 0xcccccc, 0xcc0000), cond(a<72.5));
    pixel = mix(pixel, vec4(0x007830, 0x303030, 0x303030, 0x780000), cond(a<73.5));
    pixel = mix(pixel, vec4(0x001e0c, 0x0c0c0c, 0xcccccc, 0x780000), cond(a<74.5));
    pixel = mix(pixel, vec4(0x00e666, 0x6c6c78, 0x6c6c66, 0xe60000), cond(a<75.5));
    pixel = mix(pixel, vec4(0x00f060, 0x606060, 0x626666, 0xfe0000), cond(a<76.5));
    pixel = mix(pixel, vec4(0x00c6ee, 0xfefed6, 0xc6c6c6, 0xc60000), cond(a<77.5));
    pixel = mix(pixel, vec4(0x00c6c6, 0xe6f6fe, 0xdecec6, 0xc60000), cond(a<78.5));
    pixel = mix(pixel, vec4(0x00386c, 0xc6c6c6, 0xc6c66c, 0x380000), cond(a<79.5));
    pixel = mix(pixel, vec4(0x00fc66, 0x66667c, 0x606060, 0xf00000), cond(a<80.5));
    pixel = mix(pixel, vec4(0x00386c, 0xc6c6c6, 0xcede7c, 0x0c1e00), cond(a<81.5));
    pixel = mix(pixel, vec4(0x00fc66, 0x66667c, 0x6c6666, 0xe60000), cond(a<82.5));
    pixel = mix(pixel, vec4(0x0078cc, 0xccc070, 0x18cccc, 0x780000), cond(a<83.5));
    pixel = mix(pixel, vec4(0x00fcb4, 0x303030, 0x303030, 0x780000), cond(a<84.5));
    pixel = mix(pixel, vec4(0x00cccc, 0xcccccc, 0xcccccc, 0x780000), cond(a<85.5));
    pixel = mix(pixel, vec4(0x00cccc, 0xcccccc, 0xcccc78, 0x300000), cond(a<86.5));
    pixel = mix(pixel, vec4(0x00c6c6, 0xc6c6d6, 0xd66c6c, 0x6c0000), cond(a<87.5));
    pixel = mix(pixel, vec4(0x00cccc, 0xcc7830, 0x78cccc, 0xcc0000), cond(a<88.5));
    pixel = mix(pixel, vec4(0x00cccc, 0xcccc78, 0x303030, 0x780000), cond(a<89.5));
    pixel = mix(pixel, vec4(0x00fece, 0x981830, 0x6062c6, 0xfe0000), cond(a<90.5));
    pixel = mix(pixel, vec4(0x003c30, 0x303030, 0x303030, 0x3c0000), cond(a<91.5));
    pixel = mix(pixel, vec4(0x000080, 0xc06030, 0x180c06, 0x020000), cond(a<92.5));
    pixel = mix(pixel, vec4(0x003c0c, 0x0c0c0c, 0x0c0c0c, 0x3c0000), cond(a<93.5));
    pixel = mix(pixel, vec4(0x10386c, 0xc60000, 0x000000, 0x000000), cond(a<94.5));
    pixel = mix(pixel, vec4(0x000000, 0x000000, 0x000000, 0x00ff00), cond(a<95.5));
    pixel = mix(pixel, vec4(0x303018, 0x000000, 0x000000, 0x000000), cond(a<96.5));
    pixel = mix(pixel, vec4(0x000000, 0x00780c, 0x7ccccc, 0x760000), cond(a<97.5));
    pixel = mix(pixel, vec4(0x00e060, 0x607c66, 0x666666, 0xdc0000), cond(a<98.5));
    pixel = mix(pixel, vec4(0x000000, 0x0078cc, 0xc0c0cc, 0x780000), cond(a<99.5));
    pixel = mix(pixel, vec4(0x001c0c, 0x0c7ccc, 0xcccccc, 0x760000), cond(a<100.5));
    pixel = mix(pixel, vec4(0x000000, 0x0078cc, 0xfcc0cc, 0x780000), cond(a<101.5));
    pixel = mix(pixel, vec4(0x00386c, 0x6060f8, 0x606060, 0xf00000), cond(a<102.5));
    pixel = mix(pixel, vec4(0x000000, 0x0076cc, 0xcccc7c, 0x0ccc78), cond(a<103.5));
    pixel = mix(pixel, vec4(0x00e060, 0x606c76, 0x666666, 0xe60000), cond(a<104.5));
    pixel = mix(pixel, vec4(0x001818, 0x007818, 0x181818, 0x7e0000), cond(a<105.5));
    pixel = mix(pixel, vec4(0x000c0c, 0x003c0c, 0x0c0c0c, 0xcccc78), cond(a<106.5));
    pixel = mix(pixel, vec4(0x00e060, 0x60666c, 0x786c66, 0xe60000), cond(a<107.5));
    pixel = mix(pixel, vec4(0x007818, 0x181818, 0x181818, 0x7e0000), cond(a<108.5));
    pixel = mix(pixel, vec4(0x000000, 0x00fcd6, 0xd6d6d6, 0xc60000), cond(a<109.5));
    pixel = mix(pixel, vec4(0x000000, 0x00f8cc, 0xcccccc, 0xcc0000), cond(a<110.5));
    pixel = mix(pixel, vec4(0x000000, 0x0078cc, 0xcccccc, 0x780000), cond(a<111.5));
    pixel = mix(pixel, vec4(0x000000, 0x00dc66, 0x666666, 0x7c60f0), cond(a<112.5));
    pixel = mix(pixel, vec4(0x000000, 0x0076cc, 0xcccccc, 0x7c0c1e), cond(a<113.5));
    pixel = mix(pixel, vec4(0x000000, 0x00ec6e, 0x766060, 0xf00000), cond(a<114.5));
    pixel = mix(pixel, vec4(0x000000, 0x0078cc, 0x6018cc, 0x780000), cond(a<115.5));
    pixel = mix(pixel, vec4(0x000020, 0x60fc60, 0x60606c, 0x380000), cond(a<116.5));
    pixel = mix(pixel, vec4(0x000000, 0x00cccc, 0xcccccc, 0x760000), cond(a<117.5));
    pixel = mix(pixel, vec4(0x000000, 0x00cccc, 0xcccc78, 0x300000), cond(a<118.5));
    pixel = mix(pixel, vec4(0x000000, 0x00c6c6, 0xd6d66c, 0x6c0000), cond(a<119.5));
    pixel = mix(pixel, vec4(0x000000, 0x00c66c, 0x38386c, 0xc60000), cond(a<120.5));
    pixel = mix(pixel, vec4(0x000000, 0x006666, 0x66663c, 0x0c18f0), cond(a<121.5));
    pixel = mix(pixel, vec4(0x000000, 0x00fc8c, 0x1860c4, 0xfc0000), cond(a<122.5));
    pixel = mix(pixel, vec4(0x001c30, 0x3060c0, 0x603030, 0x1c0000), cond(a<123.5));
    pixel = mix(pixel, vec4(0x001818, 0x181800, 0x181818, 0x180000), cond(a<124.5));
    pixel = mix(pixel, vec4(0x00e030, 0x30180c, 0x183030, 0xe00000), cond(a<125.5));
    pixel = mix(pixel, vec4(0x0073da, 0xce0000, 0x000000, 0x000000), cond(a<126.5));
    pixel = mix(pixel, vec4(0x000000, 0x10386c, 0xc6c6fe, 0x000000), cond(a<127.5));
    pixel = mix(pixel, vec4(0x0078cc, 0xccc0c0, 0xc0cccc, 0x783060), cond(a<128.5));
    pixel = mix(pixel, vec4(0x00cccc, 0x00cccc, 0xcccccc, 0x760000), cond(a<129.5));
    pixel = mix(pixel, vec4(0x0c1830, 0x0078cc, 0xfcc0cc, 0x780000), cond(a<130.5));
    pixel = mix(pixel, vec4(0x3078cc, 0x00780c, 0x7ccccc, 0x760000), cond(a<131.5));
    pixel = mix(pixel, vec4(0x00cccc, 0x00780c, 0x7ccccc, 0x760000), cond(a<132.5));
    pixel = mix(pixel, vec4(0xc06030, 0x00780c, 0x7ccccc, 0x760000), cond(a<133.5));
    pixel = mix(pixel, vec4(0x386c6c, 0x38f80c, 0x7ccccc, 0x760000), cond(a<134.5));
    pixel = mix(pixel, vec4(0x000000, 0x0078cc, 0xc0c0cc, 0x783060), cond(a<135.5));
    pixel = mix(pixel, vec4(0x3078cc, 0x0078cc, 0xfcc0c0, 0x7c0000), cond(a<136.5));
    pixel = mix(pixel, vec4(0x00cccc, 0x0078cc, 0xfcc0c0, 0x7c0000), cond(a<137.5));
    pixel = mix(pixel, vec4(0xc06030, 0x0078cc, 0xfcc0c0, 0x7c0000), cond(a<138.5));
    pixel = mix(pixel, vec4(0x006c6c, 0x007818, 0x181818, 0x7e0000), cond(a<139.5));
    pixel = mix(pixel, vec4(0x10386c, 0x007818, 0x181818, 0x7e0000), cond(a<140.5));
    pixel = mix(pixel, vec4(0x603018, 0x007818, 0x181818, 0x7e0000), cond(a<141.5));
    pixel = mix(pixel, vec4(0x00cc00, 0x3078cc, 0xccfccc, 0xcc0000), cond(a<142.5));
    pixel = mix(pixel, vec4(0x78cccc, 0x7878cc, 0xccfccc, 0xcc0000), cond(a<143.5));
    pixel = mix(pixel, vec4(0x0c1800, 0xfcc4c0, 0xf8c0c4, 0xfc0000), cond(a<144.5));
    pixel = mix(pixel, vec4(0x000000, 0x00fe1b, 0x7fd8d8, 0xef0000), cond(a<145.5));
    pixel = mix(pixel, vec4(0x003e78, 0xd8d8fe, 0xd8d8d8, 0xde0000), cond(a<146.5));
    pixel = mix(pixel, vec4(0x3078cc, 0x0078cc, 0xcccccc, 0x780000), cond(a<147.5));
    pixel = mix(pixel, vec4(0x00cccc, 0x0078cc, 0xcccccc, 0x780000), cond(a<148.5));
    pixel = mix(pixel, vec4(0xc06030, 0x0078cc, 0xcccccc, 0x780000), cond(a<149.5));
    pixel = mix(pixel, vec4(0x3078cc, 0x00cccc, 0xcccccc, 0x760000), cond(a<150.5));
    pixel = mix(pixel, vec4(0xc06030, 0x00cccc, 0xcccccc, 0x760000), cond(a<151.5));
    pixel = mix(pixel, vec4(0x006666, 0x006666, 0x66663c, 0x0c18f0), cond(a<152.5));
    pixel = mix(pixel, vec4(0x00cc00, 0x78cccc, 0xcccccc, 0x780000), cond(a<153.5));
    pixel = mix(pixel, vec4(0xcc00cc, 0xcccccc, 0xcccccc, 0x780000), cond(a<154.5));
    pixel = mix(pixel, vec4(0x000000, 0x0078cc, 0xdceccc, 0x780000), cond(a<155.5));
    pixel = mix(pixel, vec4(0x3c6660, 0x6060fc, 0x6060c0, 0xfe0000), cond(a<156.5));
    pixel = mix(pixel, vec4(0x003a6c, 0xced6d6, 0xd6e66c, 0xb80000), cond(a<157.5));
    pixel = mix(pixel, vec4(0x000000, 0x0000c6, 0x6c386c, 0xc60000), cond(a<158.5));
    pixel = mix(pixel, vec4(0x0e1b18, 0x187e18, 0x1818d8, 0x700000), cond(a<159.5));
    pixel = mix(pixel, vec4(0x0c1830, 0x00780c, 0x7ccccc, 0x760000), cond(a<160.5));
    pixel = mix(pixel, vec4(0x0c1830, 0x007818, 0x181818, 0x7e0000), cond(a<161.5));
    pixel = mix(pixel, vec4(0x0c1830, 0x0078cc, 0xcccccc, 0x780000), cond(a<162.5));
    pixel = mix(pixel, vec4(0x0c1830, 0x00cccc, 0xcccccc, 0x760000), cond(a<163.5));
    pixel = mix(pixel, vec4(0x0076dc, 0x00f8cc, 0xcccccc, 0xcc0000), cond(a<164.5));
    pixel = mix(pixel, vec4(0x76dc00, 0xc6e6f6, 0xdecec6, 0xc60000), cond(a<165.5));
    pixel = mix(pixel, vec4(0x0078cc, 0xcc7e00, 0xfe0000, 0x000000), cond(a<166.5));
    pixel = mix(pixel, vec4(0x0078cc, 0xcc7800, 0xfe0000, 0x000000), cond(a<167.5));
    pixel = mix(pixel, vec4(0x003030, 0x003060, 0xc0c0cc, 0x780000), cond(a<168.5));
    pixel = mix(pixel, vec4(0x003844, 0xbaaaba, 0xb2aa44, 0x380000), cond(a<169.5));
    pixel = mix(pixel, vec4(0x000000, 0x0000fc, 0x0c0c0c, 0x000000), cond(a<170.5));
    pixel = mix(pixel, vec4(0x0062e6, 0x6c7830, 0x6ec386, 0x0c1f00), cond(a<171.5));
    pixel = mix(pixel, vec4(0x0063e6, 0x6c7837, 0x6fdbb3, 0x3f0300), cond(a<172.5));
    pixel = mix(pixel, vec4(0x003030, 0x003030, 0x787878, 0x300000), cond(a<173.5));
    pixel = mix(pixel, vec4(0x000000, 0x003366, 0xcccc66, 0x330000), cond(a<174.5));
    pixel = mix(pixel, vec4(0x000000, 0x00cc66, 0x333366, 0xcc0000), cond(a<175.5));
    pixel = mix(pixel, vec4(0x249249, 0x249249, 0x249249, 0x249249), cond(a<176.5));
    pixel = mix(pixel, vec4(0x55aa55, 0xaa55aa, 0x55aa55, 0xaa55aa), cond(a<177.5));
    pixel = mix(pixel, vec4(0x6ddbb6, 0x6ddbb6, 0x6ddbb6, 0x6ddbb6), cond(a<178.5));
    pixel = mix(pixel, vec4(0x181818, 0x181818, 0x181818, 0x181818), cond(a<179.5));
    pixel = mix(pixel, vec4(0x181818, 0x1818f8, 0x181818, 0x181818), cond(a<180.5));
    pixel = mix(pixel, vec4(0x0c1800, 0x3078cc, 0xccfccc, 0xcc0000), cond(a<181.5));
    pixel = mix(pixel, vec4(0x78cc00, 0x3078cc, 0xccfccc, 0xcc0000), cond(a<182.5));
    pixel = mix(pixel, vec4(0x603000, 0x3078cc, 0xccfccc, 0xcc0000), cond(a<183.5));
    pixel = mix(pixel, vec4(0x003844, 0xbaa2a2, 0xa2ba44, 0x380000), cond(a<184.5));
    pixel = mix(pixel, vec4(0x666666, 0x66e606, 0x06e666, 0x666666), cond(a<185.5));
    pixel = mix(pixel, vec4(0x666666, 0x666666, 0x666666, 0x666666), cond(a<186.5));
    pixel = mix(pixel, vec4(0x000000, 0x00fe06, 0x06e666, 0x666666), cond(a<187.5));
    pixel = mix(pixel, vec4(0x666666, 0x66e606, 0x06fe00, 0x000000), cond(a<188.5));
    pixel = mix(pixel, vec4(0x003030, 0x78ccc0, 0xc0cc78, 0x303000), cond(a<189.5));
    pixel = mix(pixel, vec4(0xcccccc, 0xcc78fc, 0x30fc30, 0x300000), cond(a<190.5));
    pixel = mix(pixel, vec4(0x000000, 0x0000f8, 0x181818, 0x181818), cond(a<191.5));
    pixel = mix(pixel, vec4(0x181818, 0x18181f, 0x000000, 0x000000), cond(a<192.5));
    pixel = mix(pixel, vec4(0x181818, 0x1818ff, 0x000000, 0x000000), cond(a<193.5));
    pixel = mix(pixel, vec4(0x000000, 0x0000ff, 0x181818, 0x181818), cond(a<194.5));
    pixel = mix(pixel, vec4(0x181818, 0x18181f, 0x181818, 0x181818), cond(a<195.5));
    pixel = mix(pixel, vec4(0x000000, 0x0000ff, 0x000000, 0x000000), cond(a<196.5));
    pixel = mix(pixel, vec4(0x181818, 0x1818ff, 0x181818, 0x181818), cond(a<197.5));
    pixel = mix(pixel, vec4(0x0076dc, 0x00780c, 0x7ccccc, 0x760000), cond(a<198.5));
    pixel = mix(pixel, vec4(0x76dc00, 0x3078cc, 0xccfccc, 0xcc0000), cond(a<199.5));
    pixel = mix(pixel, vec4(0x666666, 0x666760, 0x607f00, 0x000000), cond(a<200.5));
    pixel = mix(pixel, vec4(0x000000, 0x007f60, 0x606766, 0x666666), cond(a<201.5));
    pixel = mix(pixel, vec4(0x666666, 0x66e700, 0x00ff00, 0x000000), cond(a<202.5));
    pixel = mix(pixel, vec4(0x000000, 0x00ff00, 0x00e766, 0x666666), cond(a<203.5));
    pixel = mix(pixel, vec4(0x666666, 0x666760, 0x606766, 0x666666), cond(a<204.5));
    pixel = mix(pixel, vec4(0x000000, 0x00ff00, 0x00ff00, 0x000000), cond(a<205.5));
    pixel = mix(pixel, vec4(0x666666, 0x66e700, 0x00e766, 0x666666), cond(a<206.5));
    pixel = mix(pixel, vec4(0x000000, 0x0000c6, 0x7c6c7c, 0xc60000), cond(a<207.5));
    pixel = mix(pixel, vec4(0xcc30d8, 0x0c067e, 0xc6c6c6, 0x7c0000), cond(a<208.5));
    pixel = mix(pixel, vec4(0x00f86c, 0x6666f6, 0x66666c, 0xf80000), cond(a<209.5));
    pixel = mix(pixel, vec4(0x78cc00, 0xfcc4c0, 0xf8c0c4, 0xfc0000), cond(a<210.5));
    pixel = mix(pixel, vec4(0x00cc00, 0xfcc4c0, 0xf8c0c4, 0xfc0000), cond(a<211.5));
    pixel = mix(pixel, vec4(0x603000, 0xfcc4c0, 0xf8c0c4, 0xfc0000), cond(a<212.5));
    pixel = mix(pixel, vec4(0x00f030, 0x3030fc, 0x000000, 0x000000), cond(a<213.5));
    pixel = mix(pixel, vec4(0x183000, 0x783030, 0x303030, 0x780000), cond(a<214.5));
    pixel = mix(pixel, vec4(0x78cc00, 0x783030, 0x303030, 0x780000), cond(a<215.5));
    pixel = mix(pixel, vec4(0x00cc00, 0x783030, 0x303030, 0x780000), cond(a<216.5));
    pixel = mix(pixel, vec4(0x181818, 0x1818f8, 0x000000, 0x000000), cond(a<217.5));
    pixel = mix(pixel, vec4(0x000000, 0x00001f, 0x181818, 0x181818), cond(a<218.5));
    pixel = mix(pixel, vec4(0xffffff, 0xffffff, 0xffffff, 0xffffff), cond(a<219.5));
    pixel = mix(pixel, vec4(0x000000, 0x000000, 0xffffff, 0xffffff), cond(a<220.5));
    pixel = mix(pixel, vec4(0x001818, 0x181800, 0x181818, 0x180000), cond(a<221.5));
    pixel = mix(pixel, vec4(0x603000, 0x783030, 0x303030, 0x780000), cond(a<222.5));
    pixel = mix(pixel, vec4(0xffffff, 0xffffff, 0x000000, 0x000000), cond(a<223.5));
    pixel = mix(pixel, vec4(0x183000, 0x78cccc, 0xcccccc, 0x780000), cond(a<224.5));
    pixel = mix(pixel, vec4(0x0078cc, 0xccd8cc, 0xccccf8, 0xc06000), cond(a<225.5));
    pixel = mix(pixel, vec4(0x78cc00, 0x78cccc, 0xcccccc, 0x780000), cond(a<226.5));
    pixel = mix(pixel, vec4(0x603000, 0x78cccc, 0xcccccc, 0x780000), cond(a<227.5));
    pixel = mix(pixel, vec4(0x0076dc, 0x0078cc, 0xcccccc, 0x780000), cond(a<228.5));
    pixel = mix(pixel, vec4(0x76dc00, 0x78cccc, 0xcccccc, 0x780000), cond(a<229.5));
    pixel = mix(pixel, vec4(0x000000, 0x006666, 0x666666, 0x7b60c0), cond(a<230.5));
    pixel = mix(pixel, vec4(0x0000e0, 0x607c66, 0x667c60, 0xf00000), cond(a<231.5));
    pixel = mix(pixel, vec4(0x00f060, 0x7c6666, 0x667c60, 0xf00000), cond(a<232.5));
    pixel = mix(pixel, vec4(0x183000, 0xcccccc, 0xcccccc, 0x780000), cond(a<233.5));
    pixel = mix(pixel, vec4(0x78cc00, 0xcccccc, 0xcccccc, 0x780000), cond(a<234.5));
    pixel = mix(pixel, vec4(0x603000, 0xcccccc, 0xcccccc, 0x780000), cond(a<235.5));
    pixel = mix(pixel, vec4(0x060c18, 0x006666, 0x66663c, 0x0c18f0), cond(a<236.5));
    pixel = mix(pixel, vec4(0x183000, 0xcccccc, 0x783030, 0x780000), cond(a<237.5));
    pixel = mix(pixel, vec4(0x00fc00, 0x000000, 0x000000, 0x000000), cond(a<238.5));
    pixel = mix(pixel, vec4(0x0c1830, 0x000000, 0x000000, 0x000000), cond(a<239.5));
    pixel = mix(pixel, vec4(0x000000, 0x0000fc, 0x000000, 0x000000), cond(a<240.5));
    pixel = mix(pixel, vec4(0x000030, 0x30fc30, 0x3000fc, 0x000000), cond(a<241.5));
    pixel = mix(pixel, vec4(0x000000, 0x0000f8, 0x0000f8, 0x000000), cond(a<242.5));
    pixel = mix(pixel, vec4(0xe03366, 0x3cf837, 0x6fdbb3, 0x3f0300), cond(a<243.5));
    pixel = mix(pixel, vec4(0x007fdb, 0xdbdb7b, 0x1b1b1b, 0x1b0000), cond(a<244.5));
    pixel = mix(pixel, vec4(0x007e63, 0x303c66, 0x663c0c, 0xc67e00), cond(a<245.5));
    pixel = mix(pixel, vec4(0x000030, 0x3000fc, 0x003030, 0x000000), cond(a<246.5));
    pixel = mix(pixel, vec4(0x000000, 0x000000, 0x000000, 0x003070), cond(a<247.5));
    pixel = mix(pixel, vec4(0x003c66, 0x66663c, 0x000000, 0x000000), cond(a<248.5));
    pixel = mix(pixel, vec4(0x00cc00, 0x000000, 0x000000, 0x000000), cond(a<249.5));
    pixel = mix(pixel, vec4(0x000000, 0x000018, 0x000000, 0x000000), cond(a<250.5));
    pixel = mix(pixel, vec4(0x003070, 0x303078, 0x000000, 0x000000), cond(a<251.5));
    pixel = mix(pixel, vec4(0x00780c, 0x380c78, 0x000000, 0x000000), cond(a<252.5));
    pixel = mix(pixel, vec4(0x00780c, 0x18307c, 0x000000, 0x000000), cond(a<253.5));
    pixel = mix(pixel, vec4(0x000000, 0xfcfcfc, 0xfcfcfc, 0x000000), cond(a<254.5));
    
    return pixel;
}


/**
 * x [0..8>
 * y [0..12>
 **/
float drawCh(in vec4 character, in float x, in float y)
{
    y = floor(11.5 - y);
    float word = 0.0;
    if (y>5.9)
    {
        if (y>8.9) word = character.x;
        else       word = character.y;
    }
    else
    {
        if (y>2.9) word = character.z;
        else       word = character.a;
    }
    float n = floor(7.0-x + 8.0*mod(y,3.0));
    return mod(floor(word/pow(2.0,n)), 2.0);
}




void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    fragColor = vec4(0.0);

    //write iResolution into BufA (store it for the next Frame)
    if (fragCoord.x<0.9 && fragCoord.y<0.9) {
        fragColor = vec4(iResolution, 1.0, 0.0);
        return;
    }

    //read iResolution of the previous Frame
    vec3 iPreviousResolution = texture2D(iChannel0, vec2(0.0)).xyz;


    vec2 pixel = vec2(floor(fragCoord.x), iResolution.y - 1.0 - floor(fragCoord.y));
    vec2 chara  = floor(pixel / CHAR_SIZE);

    if (chara.x < 31.5 && chara.y < 7.5)
    {
        if (vec3(iResolution,0.0) != iPreviousResolution)
        {
            vec4 ch = asciiToSprite(chara.x + chara.y*32.0);
            fragColor = vec4(vec3(drawCh(ch, mod(pixel.x, CHAR_SIZE.x), mod(pixel.y, CHAR_SIZE.y))), 1.0);
        }
        else
        {
            fragColor = texture2D(iChannel0, fragCoord / iResolution.xy);
        }
    }
}
