/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{html,js}",
  ],
  theme: {
    extend: {
      height: {
        '4/7': '57%',
        '3/7': '43%'
      },
      gridTemplateRows: {
        'layout-top': 'auto auto 1fr',
        'layout-bottom': 'auto auto 1fr auto'
      },
    },
  },
  plugins: [],
}
