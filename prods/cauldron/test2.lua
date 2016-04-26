GL_THING = 33191
GL_THING = 36267

stringtex = glStringToTexture(" 234abc 5 7_def_")
stringtex = glStringToTexture(" 234abc 5 7_ggg_", GL_RGBA32I_EXT, GL_INT)

stringtex = glStringToTexture(" 234abc 5 7_zzz_", GL_RGBA32F_ARB, GL_UNSIGNED_INT)

stringtex = glStringToTexture("   a", GL_RGBA32F_ARB, GL_UNSIGNED_INT)
stringtex = glStringToTexture(" a  ", GL_RGBA32F_ARB, GL_UNSIGNED_INT)
stringtex = glStringToTexture("  a ", GL_RGBA32F_ARB, GL_UNSIGNED_INT)
stringtex = glStringToTexture("   a", GL_RGBA32F_ARB, GL_UNSIGNED_INT)

setBufferName("test2.lua")


stringtex = glStringToTexture("1234efg ", GL_THING, GL_FLOAT)
