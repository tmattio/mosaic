#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H
#include FT_OUTLINE_H

#define Ft_library_val(v) (*((FT_Library *) Data_custom_val(v)))
#define Ft_face_val(v) (*((FT_Face *) Data_custom_val(v)))

/* Finalizers */
static void finalize_ft_library(value v) {
    FT_Library lib = Ft_library_val(v);
    if (lib) FT_Done_FreeType(lib);
}

static void finalize_ft_face(value v) {
    FT_Face face = Ft_face_val(v);
    if (face) {
        /* Free the memory buffer if it was allocated for a memory face */
        if (face->generic.data) {
            free(face->generic.data);
            face->generic.data = NULL;
        }
        FT_Done_Face(face);
    }
}

/* Custom operations */
static struct custom_operations ft_library_ops = {
    "freetype.library",
    finalize_ft_library,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static struct custom_operations ft_face_ops = {
    "freetype.face",
    finalize_ft_face,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

/* Initialize FreeType */
CAMLprim value ft_init(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(result);
    
    FT_Library library;
    if (FT_Init_FreeType(&library) != 0) {
        caml_failwith("Failed to initialize FreeType");
    }
    
    result = caml_alloc_custom(&ft_library_ops, sizeof(FT_Library), 0, 1);
    Ft_library_val(result) = library;
    
    CAMLreturn(result);
}

/* Load font file */
CAMLprim value ft_new_face(value ft_lib, value filename, value face_index) {
    CAMLparam3(ft_lib, filename, face_index);
    CAMLlocal1(result);
    
    FT_Library library = Ft_library_val(ft_lib);
    FT_Face face;
    
    if (FT_New_Face(library, String_val(filename), Int_val(face_index), &face) != 0) {
        caml_failwith("Failed to load font file");
    }
    
    result = caml_alloc_custom(&ft_face_ops, sizeof(FT_Face), 0, 1);
    Ft_face_val(result) = face;
    
    CAMLreturn(result);
}

/* Load font from memory */
CAMLprim value ft_new_memory_face(value ft_lib, value font_data, value face_index) {
    CAMLparam3(ft_lib, font_data, face_index);
    CAMLlocal1(result);
    
    FT_Library library = Ft_library_val(ft_lib);
    FT_Face face;
    
    /* We need to keep the font data alive as long as the face exists */
    /* For now, we'll copy it to a malloced buffer */
    size_t data_len = caml_string_length(font_data);
    unsigned char *data_copy = malloc(data_len);
    if (!data_copy) {
        caml_failwith("Out of memory");
    }
    memcpy(data_copy, String_val(font_data), data_len);
    
    if (FT_New_Memory_Face(library, data_copy, data_len, Int_val(face_index), &face) != 0) {
        free(data_copy);
        caml_failwith("Failed to load font from memory");
    }
    
    /* Store the data pointer in the face's generic.data field so we can free it later */
    face->generic.data = data_copy;
    
    result = caml_alloc_custom(&ft_face_ops, sizeof(FT_Face), 0, 1);
    Ft_face_val(result) = face;
    
    CAMLreturn(result);
}

/* Set pixel size */
CAMLprim value ft_set_pixel_sizes(value ft_face, value width, value height) {
    CAMLparam3(ft_face, width, height);
    
    FT_Face face = Ft_face_val(ft_face);
    
    if (FT_Set_Pixel_Sizes(face, Int_val(width), Int_val(height)) != 0) {
        caml_failwith("Failed to set pixel size");
    }
    
    CAMLreturn(Val_unit);
}

/* Load glyph by character code */
CAMLprim value ft_load_char(value ft_face, value char_code, value load_flags) {
    CAMLparam3(ft_face, char_code, load_flags);
    
    FT_Face face = Ft_face_val(ft_face);
    
    if (FT_Load_Char(face, Int_val(char_code), Int_val(load_flags)) != 0) {
        caml_failwith("Failed to load character");
    }
    
    CAMLreturn(Val_unit);
}

/* Load glyph by index */
CAMLprim value ft_load_glyph(value ft_face, value glyph_index, value load_flags) {
    CAMLparam3(ft_face, glyph_index, load_flags);
    
    FT_Face face = Ft_face_val(ft_face);
    
    if (FT_Load_Glyph(face, Int_val(glyph_index), Int_val(load_flags)) != 0) {
        caml_failwith("Failed to load glyph");
    }
    
    CAMLreturn(Val_unit);
}

/* Render glyph */
CAMLprim value ft_render_glyph(value ft_face, value render_mode) {
    CAMLparam2(ft_face, render_mode);
    
    FT_Face face = Ft_face_val(ft_face);
    
    if (FT_Render_Glyph(face->glyph, Int_val(render_mode)) != 0) {
        caml_failwith("Failed to render glyph");
    }
    
    CAMLreturn(Val_unit);
}

/* Get glyph bitmap and metrics */
CAMLprim value ft_get_glyph_bitmap(value ft_face) {
    CAMLparam1(ft_face);
    CAMLlocal3(result, bitmap_data, metrics);
    
    FT_Face face = Ft_face_val(ft_face);
    FT_GlyphSlot slot = face->glyph;
    FT_Bitmap* bitmap = &slot->bitmap;
    
    /* Create bitmap data */
    int size = bitmap->rows * bitmap->pitch;
    bitmap_data = caml_alloc_string(size);
    if (size > 0) {
        memcpy(Bytes_val(bitmap_data), bitmap->buffer, size);
    }
    
    /* Create metrics record */
    metrics = caml_alloc(6, 0);
    Store_field(metrics, 0, Val_int(slot->bitmap_left));
    Store_field(metrics, 1, Val_int(slot->bitmap_top));
    Store_field(metrics, 2, Val_int(slot->advance.x >> 6));
    Store_field(metrics, 3, Val_int(slot->advance.y >> 6));
    Store_field(metrics, 4, Val_int(bitmap->width));
    Store_field(metrics, 5, Val_int(bitmap->rows));
    
    /* Create result tuple */
    result = caml_alloc(3, 0);
    Store_field(result, 0, bitmap_data);
    Store_field(result, 1, metrics);
    Store_field(result, 2, Val_int(bitmap->pitch));
    
    CAMLreturn(result);
}

/* Get face metrics */
CAMLprim value ft_get_face_metrics(value ft_face) {
    CAMLparam1(ft_face);
    CAMLlocal1(result);
    
    FT_Face face = Ft_face_val(ft_face);
    
    result = caml_alloc(5, 0);
    Store_field(result, 0, Val_int(face->ascender));
    Store_field(result, 1, Val_int(face->descender));
    Store_field(result, 2, Val_int(face->height));
    Store_field(result, 3, Val_int(face->max_advance_width));
    Store_field(result, 4, Val_int(face->max_advance_height));
    
    CAMLreturn(result);
}

/* Get char index */
CAMLprim value ft_get_char_index(value ft_face, value char_code) {
    CAMLparam2(ft_face, char_code);
    
    FT_Face face = Ft_face_val(ft_face);
    FT_UInt index = FT_Get_Char_Index(face, Int_val(char_code));
    
    CAMLreturn(Val_int(index));
}

/* Get kerning between two glyphs */
CAMLprim value ft_get_kerning(value ft_face, value left_glyph, value right_glyph, value kern_mode) {
    CAMLparam4(ft_face, left_glyph, right_glyph, kern_mode);
    CAMLlocal1(result);
    
    FT_Face face = Ft_face_val(ft_face);
    FT_Vector kerning;
    
    if (FT_Get_Kerning(face, Int_val(left_glyph), Int_val(right_glyph), 
                       Int_val(kern_mode), &kerning) != 0) {
        /* No kerning or error - return (0, 0) */
        kerning.x = 0;
        kerning.y = 0;
    }
    
    result = caml_alloc(2, 0);
    Store_field(result, 0, Val_int(kerning.x));
    Store_field(result, 1, Val_int(kerning.y));
    
    CAMLreturn(result);
}

/* Check if face has kerning */
CAMLprim value ft_has_kerning(value ft_face) {
    CAMLparam1(ft_face);
    FT_Face face = Ft_face_val(ft_face);
    CAMLreturn(Val_bool(FT_HAS_KERNING(face)));
}

/* Get number of glyphs in face */
CAMLprim value ft_num_glyphs(value ft_face) {
    CAMLparam1(ft_face);
    FT_Face face = Ft_face_val(ft_face);
    CAMLreturn(Val_int(face->num_glyphs));
}

/* Get bitmap data as bigarray for zero-copy access */
CAMLprim value ft_get_glyph_bitmap_bigarray(value ft_face) {
    CAMLparam1(ft_face);
    CAMLlocal2(result, ba);
    
    FT_Face face = Ft_face_val(ft_face);
    FT_GlyphSlot slot = face->glyph;
    FT_Bitmap* bitmap = &slot->bitmap;
    
    /* Create bigarray pointing to bitmap buffer */
    intnat dims[2] = {bitmap->rows, bitmap->pitch};
    ba = caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 2, bitmap->buffer, dims);
    
    /* Create result with bigarray and metrics */
    result = caml_alloc(7, 0);
    Store_field(result, 0, ba);
    Store_field(result, 1, Val_int(slot->bitmap_left));
    Store_field(result, 2, Val_int(slot->bitmap_top));
    Store_field(result, 3, Val_int(slot->advance.x));  /* Keep in 26.6 format */
    Store_field(result, 4, Val_int(slot->advance.y));
    Store_field(result, 5, Val_int(bitmap->width));
    Store_field(result, 6, Val_int(bitmap->rows));
    
    CAMLreturn(result);
}

/* Get glyph metrics without rendering */
CAMLprim value ft_get_glyph_metrics(value ft_face) {
    CAMLparam1(ft_face);
    CAMLlocal1(result);
    
    FT_Face face = Ft_face_val(ft_face);
    FT_GlyphSlot slot = face->glyph;
    FT_Glyph_Metrics* m = &slot->metrics;
    
    result = caml_alloc(8, 0);
    Store_field(result, 0, Val_int(m->width));
    Store_field(result, 1, Val_int(m->height));
    Store_field(result, 2, Val_int(m->horiBearingX));
    Store_field(result, 3, Val_int(m->horiBearingY));
    Store_field(result, 4, Val_int(m->horiAdvance));
    Store_field(result, 5, Val_int(m->vertBearingX));
    Store_field(result, 6, Val_int(m->vertBearingY));
    Store_field(result, 7, Val_int(m->vertAdvance));
    
    CAMLreturn(result);
}

