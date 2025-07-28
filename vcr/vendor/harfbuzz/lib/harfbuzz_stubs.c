#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <stdlib.h>
#include <hb.h>
#include <hb-ft.h>

/* Custom block definitions */
#define Hb_face_val(v) (*((hb_face_t **) Data_custom_val(v)))
#define Hb_font_val(v) (*((hb_font_t **) Data_custom_val(v)))
#define Hb_buffer_val(v) (*((hb_buffer_t **) Data_custom_val(v)))

/* Finalizers */
static void finalize_hb_face(value v) {
    hb_face_t *face = Hb_face_val(v);
    if (face) hb_face_destroy(face);
}

static void finalize_hb_font(value v) {
    hb_font_t *font = Hb_font_val(v);
    if (font) hb_font_destroy(font);
}

static void finalize_hb_buffer(value v) {
    hb_buffer_t *buffer = Hb_buffer_val(v);
    if (buffer) hb_buffer_destroy(buffer);
}

/* Custom operations */
__attribute__((unused))
static struct custom_operations hb_face_ops = {
    "harfbuzz.face",
    finalize_hb_face,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static struct custom_operations hb_font_ops = {
    "harfbuzz.font",
    finalize_hb_font,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

static struct custom_operations hb_buffer_ops = {
    "harfbuzz.buffer",
    finalize_hb_buffer,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

/* FreeType integration - use the same macro as freetype_stubs.c */
#define Ft_face_val(v) (*((FT_Face *) Data_custom_val(v)))

/* Create HarfBuzz font from FreeType face */
CAMLprim value hb_ft_font_create_from_face(value ft_face) {
    CAMLparam1(ft_face);
    CAMLlocal1(result);
    
    FT_Face face = Ft_face_val(ft_face);
    hb_font_t *font = hb_ft_font_create(face, NULL);
    
    if (!font) {
        caml_failwith("Failed to create HarfBuzz font");
    }
    
    result = caml_alloc_custom(&hb_font_ops, sizeof(hb_font_t *), 0, 1);
    Hb_font_val(result) = font;
    
    CAMLreturn(result);
}

/* Create a new buffer */
CAMLprim value caml_hb_buffer_create(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(result);
    
    hb_buffer_t *buffer = hb_buffer_create();
    if (!buffer) {
        caml_failwith("Failed to create buffer");
    }
    
    result = caml_alloc_custom(&hb_buffer_ops, sizeof(hb_buffer_t *), 0, 1);
    Hb_buffer_val(result) = buffer;
    
    CAMLreturn(result);
}

/* Add UTF-8 text to buffer */
CAMLprim value caml_hb_buffer_add_utf8(value buffer, value text) {
    CAMLparam2(buffer, text);
    
    hb_buffer_t *buf = Hb_buffer_val(buffer);
    const char *str = String_val(text);
    int len = caml_string_length(text);
    
    hb_buffer_add_utf8(buf, str, len, 0, len);
    
    CAMLreturn(Val_unit);
}

/* Set buffer direction */
CAMLprim value caml_hb_buffer_set_direction(value buffer, value dir) {
    CAMLparam2(buffer, dir);
    
    hb_buffer_t *buf = Hb_buffer_val(buffer);
    hb_direction_t direction = HB_DIRECTION_LTR; // Default
    
    if (Int_val(dir) == 1) direction = HB_DIRECTION_RTL;
    else if (Int_val(dir) == 2) direction = HB_DIRECTION_TTB;
    else if (Int_val(dir) == 3) direction = HB_DIRECTION_BTT;
    
    hb_buffer_set_direction(buf, direction);
    
    CAMLreturn(Val_unit);
}

/* Shape the buffer */
CAMLprim value caml_hb_shape(value font, value buffer) {
    CAMLparam2(font, buffer);
    
    hb_font_t *hb_font = Hb_font_val(font);
    hb_buffer_t *hb_buffer = Hb_buffer_val(buffer);
    
    hb_shape(hb_font, hb_buffer, NULL, 0);
    
    CAMLreturn(Val_unit);
}

/* Get glyph info and positions */
CAMLprim value hb_buffer_get_glyph_infos_and_positions(value buffer) {
    CAMLparam1(buffer);
    CAMLlocal3(result, glyph_info, glyph_pos);
    
    hb_buffer_t *buf = Hb_buffer_val(buffer);
    unsigned int glyph_count;
    
    hb_glyph_info_t *glyph_infos = hb_buffer_get_glyph_infos(buf, &glyph_count);
    hb_glyph_position_t *glyph_positions = hb_buffer_get_glyph_positions(buf, &glyph_count);
    
    result = caml_alloc(glyph_count, 0);
    
    for (unsigned int i = 0; i < glyph_count; i++) {
        glyph_info = caml_alloc(5, 0);
        Store_field(glyph_info, 0, Val_int(glyph_infos[i].codepoint));
        Store_field(glyph_info, 1, Val_int(glyph_positions[i].x_advance));
        Store_field(glyph_info, 2, Val_int(glyph_positions[i].y_advance));
        Store_field(glyph_info, 3, Val_int(glyph_positions[i].x_offset));
        Store_field(glyph_info, 4, Val_int(glyph_positions[i].y_offset));
        
        Store_field(result, i, glyph_info);
    }
    
    CAMLreturn(result);
}

/* Clear buffer */
CAMLprim value caml_hb_buffer_clear(value buffer) {
    CAMLparam1(buffer);
    
    hb_buffer_t *buf = Hb_buffer_val(buffer);
    hb_buffer_clear_contents(buf);
    
    CAMLreturn(Val_unit);
}

/* Get font metrics */
CAMLprim value hb_font_get_metrics(value font) {
    CAMLparam1(font);
    CAMLlocal1(result);
    
    hb_font_t *hb_font = Hb_font_val(font);
    hb_font_extents_t extents;
    
    hb_font_get_h_extents(hb_font, &extents);
    
    result = caml_alloc(3, 0);
    Store_field(result, 0, Val_int(extents.ascender));
    Store_field(result, 1, Val_int(extents.descender));
    Store_field(result, 2, Val_int(extents.line_gap));
    
    CAMLreturn(result);
}

/* Set buffer language */
CAMLprim value caml_hb_buffer_set_language(value buffer, value lang) {
    CAMLparam2(buffer, lang);
    
    hb_buffer_t *buf = Hb_buffer_val(buffer);
    hb_language_t language = hb_language_from_string(String_val(lang), -1);
    hb_buffer_set_language(buf, language);
    
    CAMLreturn(Val_unit);
}

/* Set buffer script */
CAMLprim value caml_hb_buffer_set_script(value buffer, value script) {
    CAMLparam2(buffer, script);
    
    hb_buffer_t *buf = Hb_buffer_val(buffer);
    hb_buffer_set_script(buf, Int_val(script));
    
    CAMLreturn(Val_unit);
}

/* Get buffer length */
CAMLprim value caml_hb_buffer_get_length(value buffer) {
    CAMLparam1(buffer);
    
    hb_buffer_t *buf = Hb_buffer_val(buffer);
    unsigned int len = hb_buffer_get_length(buf);
    
    CAMLreturn(Val_int(len));
}

/* Shape with features */
CAMLprim value caml_hb_shape_with_features(value font, value buffer, value features) {
    CAMLparam3(font, buffer, features);
    
    hb_font_t *hb_font = Hb_font_val(font);
    hb_buffer_t *hb_buffer = Hb_buffer_val(buffer);
    
    int num_features = Wosize_val(features);
    hb_feature_t *feature_array = NULL;
    
    if (num_features > 0) {
        feature_array = malloc(sizeof(hb_feature_t) * num_features);
        for (int i = 0; i < num_features; i++) {
            value feat = Field(features, i);
            const char *str = String_val(feat);
            hb_feature_from_string(str, -1, &feature_array[i]);
        }
    }
    
    hb_shape(hb_font, hb_buffer, feature_array, num_features);
    
    if (feature_array) free(feature_array);
    
    CAMLreturn(Val_unit);
}

/* Get font scale */
CAMLprim value caml_hb_font_get_scale(value font) {
    CAMLparam1(font);
    CAMLlocal1(result);
    
    hb_font_t *hb_font = Hb_font_val(font);
    int x_scale, y_scale;
    
    hb_font_get_scale(hb_font, &x_scale, &y_scale);
    
    result = caml_alloc(2, 0);
    Store_field(result, 0, Val_int(x_scale));
    Store_field(result, 1, Val_int(y_scale));
    
    CAMLreturn(result);
}

/* Set font scale */
CAMLprim value caml_hb_font_set_scale(value font, value x_scale, value y_scale) {
    CAMLparam3(font, x_scale, y_scale);
    
    hb_font_t *hb_font = Hb_font_val(font);
    hb_font_set_scale(hb_font, Int_val(x_scale), Int_val(y_scale));
    
    CAMLreturn(Val_unit);
}