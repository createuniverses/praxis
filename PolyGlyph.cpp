#include "PolyGlyph.h"
#include "assert.h"
#include <iostream>
//#include "Unicode.h"

PolyGlyph::PolyGlyph(const string &ttffilename)
{
	FT_Error error;
	error = FT_Init_FreeType(&m_Library);
	error = FT_New_Face(m_Library, ttffilename.c_str(), 0, &m_Face);

	if (error)
	{
  	        cerr<<"PolyGlyph::PolyGlyph: could not load font: "<<ttffilename<<endl;
		assert(0);
	}

	// use 5pt at 100dpi
	error = FT_Set_Char_Size(m_Face, 50 * 64, 0, 100, 0);
	m_Slot = m_Face->glyph;
}

PolyGlyph::~PolyGlyph()
{
    FT_Done_Face(m_Face);
    FT_Done_FreeType(m_Library);
}

void PolyGlyph::ClearCache()
{
    m_Cache.clear();
}

void PolyGlyph::Render(wchar_t ch, float r, float g, float b, float a,
		float dx /* = 0 */, float dy /* = 0 */)
{
	glPushMatrix();
	glTranslatef(dx, dy, 0);
    map<wchar_t,int>::iterator i = m_Cache.find(ch);
	if (i!=m_Cache.end())
	{
        glColor4f(1-r, 1-g, 1-b, a*0.5);
        glCallList(i->second+1);
		glColor4f(r, g, b, a);
		glCallList(i->second);
	}
	else
	{
		FT_Error error;
		error = FT_Load_Char(m_Face, ch, FT_LOAD_DEFAULT);
		if (error) return;

		int glList = glGenLists(2);
		GlyphGeometry* geo = new GlyphGeometry;
		BuildGeometry(m_Slot,*geo);

		glNewList(glList+1, GL_COMPILE);
        RenderOutline(m_Slot);
		glEndList();

		glNewList(glList, GL_COMPILE);
		RenderGeometry(*geo);
		glTranslatef(m_Slot->metrics.horiAdvance,0,0);
		glEndList();
		delete geo;

		m_Cache[ch]=glList;
		glColor4f(1-r, 1-g, 1-b, a*0.5);
		glCallList(glList+1);
		glColor4f(r, g, b, a);
		glCallList(glList);
	}
	glPopMatrix();
	glTranslatef(m_Slot->metrics.horiAdvance,0,0);
}

float PolyGlyph::CharacterWidth(wchar_t ch)
{
	FT_Error error;
	error = FT_Load_Char(m_Face, ch, FT_LOAD_DEFAULT);
    if (error) return 0;
	return m_Slot->metrics.horiAdvance;
}

float PolyGlyph::CharacterHeight(wchar_t ch)
{
	FT_Error error;
	error = FT_Load_Char(m_Face, ch, FT_LOAD_DEFAULT);
    if (error) return 0;
	return m_Slot->metrics.vertAdvance;
}

void PolyGlyph::BuildGeometry(const FT_GlyphSlot glyph, GlyphGeometry &geo)
{
	vector<GlyphGeometry::Vec3<double> > points;
	GLUtesselator* t = gluNewTess();

#if (defined __APPLE__) && (MAC_OS_X_VERSION_MAX_ALLOWED <= MAC_OS_X_VERSION_10_4)
	gluTessCallback(t, GLU_TESS_BEGIN_DATA, (GLvoid (*)(...))PolyGlyph::TessBegin);
	gluTessCallback(t, GLU_TESS_VERTEX_DATA, (GLvoid (*)(...))PolyGlyph::TessVertex);
	gluTessCallback(t, GLU_TESS_COMBINE_DATA, (GLvoid (*)(...))PolyGlyph::TessCombine);
	gluTessCallback(t, GLU_TESS_END_DATA, (GLvoid (*)(...))PolyGlyph::TessEnd);
	gluTessCallback(t, GLU_TESS_ERROR_DATA, (GLvoid (*)(...))PolyGlyph::TessError);
#else
#ifdef WIN32	
	gluTessCallback(t, GLU_TESS_BEGIN_DATA, (GLvoid (__stdcall *)())PolyGlyph::TessBegin);
	gluTessCallback(t, GLU_TESS_VERTEX_DATA, (GLvoid (__stdcall *)())PolyGlyph::TessVertex);
	gluTessCallback(t, GLU_TESS_COMBINE_DATA, (GLvoid (__stdcall *)())PolyGlyph::TessCombine);
	gluTessCallback(t, GLU_TESS_END_DATA, (GLvoid (__stdcall *)())PolyGlyph::TessEnd);
	gluTessCallback(t, GLU_TESS_ERROR_DATA, (GLvoid (__stdcall *)())PolyGlyph::TessError);
#else
	gluTessCallback(t, GLU_TESS_BEGIN_DATA, (void (*)())PolyGlyph::TessBegin);
	gluTessCallback(t, GLU_TESS_VERTEX_DATA, (void (*)())PolyGlyph::TessVertex);
	gluTessCallback(t, GLU_TESS_COMBINE_DATA, (void (*)())PolyGlyph::TessCombine);
	gluTessCallback(t, GLU_TESS_END_DATA, (void (*)())PolyGlyph::TessEnd);
	gluTessCallback(t, GLU_TESS_ERROR_DATA, (void (*)())PolyGlyph::TessError);
#endif
#endif

	gluTessProperty(t, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_NONZERO);

	gluTessProperty(t, GLU_TESS_TOLERANCE, 0);
	gluTessNormal(t, 0.0f, 0.0f, 1.0f);
	gluTessBeginPolygon(t, &geo);

	unsigned int start=0;
	for(int c=0; c<glyph->outline.n_contours; c++)
	{
		unsigned int end = glyph->outline.contours[c]+1;
		for(unsigned int p = start; p<end; p++)
		{
			points.push_back(GlyphGeometry::Vec3<double>(glyph->outline.points[p].x,glyph->outline.points[p].y,0));
		}
		start=end;
	}

	start=0;
	for(int c=0; c<glyph->outline.n_contours; c++)
	{
		unsigned int end = glyph->outline.contours[c]+1;
		gluTessBeginContour(t);
		for(unsigned int p = start; p<end; p++)
		{
			gluTessVertex(t, &points[p].x,
			                 &points[p].x);
		}
		start=end;
		gluTessEndContour(t);
	}

	gluTessEndPolygon(t);
	gluDeleteTess(t);
	
	for (vector<double*>::iterator i=geo.m_CombinedVerts.begin(); i!=geo.m_CombinedVerts.end(); ++i)
	{
		delete[] *i;
	}
	geo.m_CombinedVerts.clear();
}

void __stdcall PolyGlyph::TessError( GLenum errCode, GlyphGeometry* geo)
{
    geo->m_Error=errCode;
}


void __stdcall PolyGlyph::TessVertex( void* data, GlyphGeometry* geo)
{
	double *ptr = (double*)data;
    geo->m_Meshes[geo->m_Meshes.size()-1].m_Data.push_back(GlyphGeometry::Vec3<float>(ptr[0],ptr[1],ptr[2]));
}


void __stdcall PolyGlyph::TessCombine( double coords[3], void* vertex_data[4], float weight[4], void** outData, GlyphGeometry* geo)
{
	double *out = new double[3];
	out[0]=coords[0];
	out[1]=coords[1];
	out[2]=coords[2];
	geo->m_CombinedVerts.push_back(out);
	*outData=out;
}


void __stdcall PolyGlyph::TessBegin( GLenum type, GlyphGeometry* geo)
{
	geo->m_Meshes.push_back(GlyphGeometry::Mesh(type));
}


void __stdcall PolyGlyph::TessEnd(GlyphGeometry* geo)
{
}

void PolyGlyph::RenderOutline(const FT_GlyphSlot glyph)
{
	unsigned int start=0;
    glLineWidth(2);
	for(int c=0; c<glyph->outline.n_contours; c++)
	{
		glBegin(GL_LINE_LOOP);
		unsigned int end = glyph->outline.contours[c]+1;
		for(unsigned int p = start; p<end; p++)
		{
			glVertex3f(glyph->outline.points[p].x, glyph->outline.points[p].y, 0);
		}
		glEnd();
		start=end;
	}
}

void PolyGlyph::RenderGeometry(const GlyphGeometry &geo)
{
	/*for (vector<GlyphGeometry::Mesh>::const_iterator i=geo.m_Meshes.begin(); i!=geo.m_Meshes.end(); i++)
	{
		glVertexPointer(3,GL_FLOAT,sizeof(float)*3,(void*)(&i->m_Data.begin()->x));
		glDrawArrays(i->m_Type,0,i->m_Data.size());
	}*/

	// i don't like em, but display lists + glbegin are faster than vertex arrays
	for (vector<GlyphGeometry::Mesh>::const_iterator i=geo.m_Meshes.begin(); i!=geo.m_Meshes.end(); i++)
	{
		glBegin(i->m_Type);
		for (vector<GlyphGeometry::Vec3<float> >::const_iterator p=i->m_Data.begin(); p!=i->m_Data.end(); p++)
		{
			glVertex3f(p->x,p->y,p->z);
		}
		glEnd();
	}
}

