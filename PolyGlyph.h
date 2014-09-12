#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <map>

#include "GL/openglut.h"

#include <ft2build.h>
#include FT_FREETYPE_H

#ifndef POLY_GLYPH_H
#define POLY_GLYPH_H

using namespace std;


class GlyphGeometry
{
public:
	GlyphGeometry() {}
	~GlyphGeometry() {}

	template<class T>
	class Vec3
	{
	public:
		Vec3(T a, T b, T c) : x(a), y(b), z(c) {}
		T x,y,z;
	};

	class Mesh
	{
	public:
		Mesh(GLenum type) : m_Type(type) {}
		~Mesh() {}

		GLenum m_Type;
		vector<Vec3<float> > m_Data;
	};

	GLenum m_Error;
	vector<Mesh> m_Meshes;
	vector<double*> m_CombinedVerts;
};

class PolyGlyph
{
public:
	PolyGlyph(const string &ttffile);
	~PolyGlyph();

    void Render(wchar_t ch, float r, float g, float b, float a, float dx = 0, float dy = 0);
    float CharacterWidth(wchar_t ch);
    float CharacterHeight(wchar_t ch);

private:

	void BuildGeometry(const FT_GlyphSlot glyph, GlyphGeometry &geo);
	void RenderGeometry(const GlyphGeometry &geo);
	void RenderOutline(const FT_GlyphSlot glyph);

	FT_Library    m_Library;
	FT_Face       m_Face;
	FT_GlyphSlot  m_Slot;

    map<wchar_t,int> m_Cache;

#ifndef WIN32 
#define __stdcall
#endif

	static void __stdcall TessError(GLenum errCode, GlyphGeometry* geo);
	static void __stdcall TessVertex(void* data, GlyphGeometry* geo);
	static void __stdcall TessCombine(double coords[3], void* vertex_data[4], float weight[4], void** outData, GlyphGeometry* geo);
	static void __stdcall TessBegin(GLenum type, GlyphGeometry* geo);
	static void __stdcall TessEnd(GlyphGeometry* geo);

};

#endif
