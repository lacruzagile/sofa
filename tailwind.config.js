const defaultTheme = require('tailwindcss/defaultTheme')

module.exports = {
  mode: "jit",
  content: ["./*.html", "./src/**/*.purs"],
  theme: {
    extend: {
      colors: {
        // Text colors:
        //   - default: stormy-500
        //   - inverted: snow-100
        //   - muted: stormy-300
        //   - link: tropical-500
        //   - invalid: raspberry-500

        honey: {
          700: "#CC8800",
          600: "#FFAA00",
          500: "#FFBE3C",
          400: "#FFCC66",
          300: "#FFDD99",
          200: "#FFEECC",
          100: "#FFF7E6",
        },
        tropical: {
          700: "#003D3D",
          600: "#005757",
          500: "#007171",
          400: "#338D8D",
          300: "#66AAAA",
          200: "#99C6C6",
          100: "#CCE3E3",
          50: "#E5F1F1",
        },
        stormy: {
          600: "#061927",
          500: "#0A273D",
          400: "#3B5264",
          300: "#677784",
          200: "#7F8F99",
          100: "#9CA8B0",
        },
        snow: {
          800: "#C6CDD2",
          700: "#D4DADD",
          600: "#E3E6E8",
          500: "#F1F3F4",
          200: "#FAFAFA",       /* Not actually in Nectary. */
          100: "#FFFFFF",
        },
        raspberry: {
          700: "#881125",
          600: "#B61631",
          500: "#E31C3D",
          400: "#E94964",
          300: "#EE778B",
          200: "#F4A4B1",
          100: "#F9D2D8",
          50: "#FCE8EB",
        },
        error: {
          800: "#57000E",
          500: "#E31C3D",
          200: "#FCD7D4",
        },
        success: {
          700: "#005419",
          500: "#2E8540",
          200: "#D7F1D8",
        },
        informative: {
          700: "#003B7E",
          500: "#2071CE",
          200: "#D5E5F8",
        },
        warning: {
          700: "#9C2E00",
          500: "#F35B1C",
          200: "#FFE8D6",
        },
        night: {
          400: "#3247E9",
          200: "#D1D6FA",
        },
        aqua: {
          400: "#3DAED8",
          200: "#AADBEE",
        },
        grass: {
          400: "#39B93D",
          200: "#B4E4B5",
        },
        dirt: {
          400: "#828282",
          200: "#E0DDDC",
        },
        berry: {
          400: "#F95252",
          200: "#FCA7A7",
        },
        candy: {
          400: "#E467C3",
          200: "#F6CBEA",
        },
        mud: {
          400: "#8B6559",
          200: "#D7C6C1",
        },
        orange: {
          400: "#FF8C34",
          200: "#FFD4B3",
        },
        bolt: {
          400: "#FFBE3C",
          200: "#FFE6B3",
        },
      },
      fontFamily: {
        sans: [
          'Gilroy',
          ...defaultTheme.fontFamily.sans,
        ]
      },
      fontSize: {
        '4xl': '2.5rem',
      },
      maxHeight: {
        '128': '32rem',
      },
      maxWidth: {
        '96': '24rem',
        '128': '32rem',
      },
      minWidth: {
        '96': '24rem',
        '128': '32rem',
      },
    },
  },
  variants: {},
  plugins: [],
};
