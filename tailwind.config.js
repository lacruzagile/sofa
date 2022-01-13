module.exports = {
  mode: "jit",
  /* content: ["./*.html", "./src/ ** /*.purs"], */
  content: ["./*.html", "output/Css/index.js"],
  theme: {
    extend: {
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
