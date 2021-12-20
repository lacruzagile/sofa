module.exports = {
  mode: "jit",
  /* content: ["./*.html", "./src/ ** /*.purs"], */
  content: ["./*.html", "output/Css/index.js"],
  theme: {
    extend: {
      minWidth: {
        '96': '24rem',
        '128': '32rem',
      },
    },
  },
  variants: {},
  plugins: [],
};
