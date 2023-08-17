/** @type {import('jest-environment-puppeteer').JestPuppeteerConfig} */
module.exports = {
  server: {
    command: "npm run start",
    launchTimeout: 10000,
    host: "localhost",
    port: 1234,
  },
};
