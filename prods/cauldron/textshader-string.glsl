// --------------------------------------------------------------
//
//  This Shader codes a string into a texture.
//  
// --------------------------------------------------------------

//  Created by Bart Verheijen 2016
//  License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.



vec4 myText(vec2 v)
{
    if (v.y < 0.5 || v.y > 22.5 || v.x > 2.5) {
        return vec4(0x202020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 1.5) {
        if (v.x < 0.5)  return vec4(0x4c6f72, 0x656d20, 0x697073, 0x756d20);
        if (v.x < 1.5)  return vec4(0x646f6c, 0x6f7220, 0x736974, 0x20616d);
        if (v.x < 2.5)  return vec4(0x65742c, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 2.5) {
        if (v.x < 0.5)  return vec4(0x202063, 0x6f6e73, 0x656374, 0x657475);
        if (v.x < 1.5)  return vec4(0x722061, 0x646970, 0x697363, 0x696e67);
        if (v.x < 2.5)  return vec4(0x202020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 3.5) {
        if (v.x < 0.5)  return vec4(0x656c69, 0x742c20, 0x736564, 0x20646f);
        if (v.x < 1.5)  return vec4(0x206569, 0x75736d, 0x6f6420, 0x74656d);
        if (v.x < 2.5)  return vec4(0x706f72, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 4.5) {
        if (v.x < 0.5)  return vec4(0x202069, 0x6e6369, 0x646964, 0x756e74);
        if (v.x < 1.5)  return vec4(0x207574, 0x206c61, 0x626f72, 0x652065);
        if (v.x < 2.5)  return vec4(0x742020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 5.5) {
        if (v.x < 0.5)  return vec4(0x202020, 0x20646f, 0x6c6f72, 0x65206d);
        if (v.x < 1.5)  return vec4(0x61676e, 0x612061, 0x6c6971, 0x75612e);
        if (v.x < 2.5)  return vec4(0x202020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 6.5) {
        if (v.x < 0.5)  return vec4(0x202055, 0x742065, 0x6e696d, 0x206164);
        if (v.x < 1.5)  return vec4(0x206d69, 0x6e696d, 0x207665, 0x6e6961);
        if (v.x < 2.5)  return vec4(0x6d2c20, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 7.5) {
        if (v.x < 0.5)  return vec4(0x207175, 0x697320, 0x6e6f73, 0x747275);
        if (v.x < 1.5)  return vec4(0x642065, 0x786572, 0x636974, 0x617469);
        if (v.x < 2.5)  return vec4(0x6f6e20, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 8.5) {
        if (v.x < 0.5)  return vec4(0x202075, 0x6c6c61, 0x6d636f, 0x206c61);
        if (v.x < 1.5)  return vec4(0x626f72, 0x697320, 0x6e6973, 0x692075);
        if (v.x < 2.5)  return vec4(0x742020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 9.5) {
        if (v.x < 0.5)  return vec4(0x202020, 0x616c69, 0x717569, 0x702065);
        if (v.x < 1.5)  return vec4(0x782065, 0x612063, 0x6f6d6d, 0x6f646f);
        if (v.x < 2.5)  return vec4(0x202020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 10.5) {
        if (v.x < 0.5)  return vec4(0x202020, 0x202020, 0x202020, 0x636f6e);
        if (v.x < 1.5)  return vec4(0x736571, 0x756174, 0x2e2020, 0x202020);
        if (v.x < 2.5)  return vec4(0x202020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 13.5) {
        return vec4(0x202020);
    }
    if (v.y < 14.5) {
        if (v.x < 0.5)  return vec4(0x204475, 0x697320, 0x617574, 0x652069);
        if (v.x < 1.5)  return vec4(0x727572, 0x652064, 0x6f6c6f, 0x722069);
        if (v.x < 2.5)  return vec4(0x6e2020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 15.5) {
        if (v.x < 0.5)  return vec4(0x726570, 0x726568, 0x656e64, 0x657269);
        if (v.x < 1.5)  return vec4(0x742069, 0x6e2076, 0x6f6c75, 0x707461);
        if (v.x < 2.5)  return vec4(0x746520, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 16.5) {
        if (v.x < 0.5)  return vec4(0x207665, 0x6c6974, 0x206573, 0x736520);
        if (v.x < 1.5)  return vec4(0x63696c, 0x6c756d, 0x20646f, 0x6c6f72);
        if (v.x < 2.5)  return vec4(0x652020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 17.5) {
        if (v.x < 0.5)  return vec4(0x206575, 0x206675, 0x676961, 0x74206e);
        if (v.x < 1.5)  return vec4(0x756c6c, 0x612070, 0x617269, 0x617475);
        if (v.x < 2.5)  return vec4(0x722e20, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 18.5) {
        if (v.x < 0.5)  return vec4(0x202045, 0x786365, 0x707465, 0x757220);
        if (v.x < 1.5)  return vec4(0x73696e, 0x74206f, 0x636361, 0x656361);
        if (v.x < 2.5)  return vec4(0x742020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 19.5) {
        if (v.x < 0.5)  return vec4(0x202063, 0x757069, 0x646174, 0x617420);
        if (v.x < 1.5)  return vec4(0x6e6f6e, 0x207072, 0x6f6964, 0x656e74);
        if (v.x < 2.5)  return vec4(0x2c2020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 20.5) {
        if (v.x < 0.5)  return vec4(0x207375, 0x6e7420, 0x696e20, 0x63756c);
        if (v.x < 1.5)  return vec4(0x706120, 0x717569, 0x206f66, 0x666963);
        if (v.x < 2.5)  return vec4(0x696120, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 21.5) {
        if (v.x < 0.5)  return vec4(0x202064, 0x657365, 0x72756e, 0x74206d);
        if (v.x < 1.5)  return vec4(0x6f6c6c, 0x697420, 0x616e69, 0x6d2069);
        if (v.x < 2.5)  return vec4(0x642020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 22.5) {
        if (v.x < 0.5)  return vec4(0x202020, 0x202020, 0x202065, 0x737420);
        if (v.x < 1.5)  return vec4(0x6c6162, 0x6f7275, 0x6d2e20, 0x202020);
        if (v.x < 2.5)  return vec4(0x202020, 0x202020, 0x202020, 0x202020);
    }

    return vec4(0x202020, 0x202020, 0x202020, 0x202020);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    //fragColor = vec4(0x102020);
    //return;

    //write iResolution into BufA (store it for the next Frame)
    if (fragCoord.x<0.9 && fragCoord.y<0.9) {
        fragColor = vec4(iResolution, 1.0, 0.0);
        return;
    }

    //read iResolution of the previous Frame
    vec3 iPreviousResolution = texture2D(iChannel0, vec2(0.0)).xyz;


    if (vec3(iResolution,0.0) != iPreviousResolution)
    {
        fragColor = myText(floor(fragCoord));
    }
    else
    {
        fragColor = texture2D(iChannel0, fragCoord / iResolution.xy);
    }
}

