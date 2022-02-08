module.exports = {
  mode: "jit",
  /* content: ["./*.html", "./src/ ** /*.purs"], */
  content: ["./*.html", "output/Css/index.js"],
  theme: {
    extend: {
      colors: {
        honey: {
          100: '#fff7e6',
          200: '#ffeecc',
          300: '#ffdd99',
          400: '#ffcc66',
          500: '#ffbe3c',
          600: '#ffaa00',
          700: '#cc8800',
        },
        raspberry: {
          100: '#f9d2d8',
          200: '#f4a4b1',
          300: '#ee778b',
          400: '#e94964',
          500: '#e31c3d',
          600: '#b61631',
          700: '#881125',
        },
        tropical: {
          100: '#cce3e3',
          200: '#99c6c6',
          300: '#66aaaa',
          400: '#338d8d',
          500: '#007171',
          600: '#005757',
          700: '#003d3d',
        },

        stormy: {
          100: '#9ca8b0',
          200: '#7f8f99',
          300: '#677784',
          400: '#3b5264',
          500: '#0a273d',
          600: '#061927',
        },
        snow: {
          400: '#ffffff',
          500: '#f1f3f4',
          600: '#e3e6e8',
          700: '#d4dadd',
          800: '#c6cdd2',
        },
      },
      maxHeight: {
        '128': '32rem',
      },
      maxWidth: {
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
