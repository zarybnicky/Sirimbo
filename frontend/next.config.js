const withPreact = require('next-plugin-preact');
const withReactSvg = require('next-react-svg');
const path = require('path');

module.exports = withReactSvg(
  withPreact({
    include: path.resolve(__dirname, 'public/'),
    experimental: { outputStandalone: true },
  }),
);
