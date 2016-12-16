const path = require('path');
const webpack = require('webpack');
const getClientEnvironment = require('./config/env');

// Get environment variables to inject into our app.
const env = getClientEnvironment();

// Assert this just to be safe.
// Development builds of React are slow and not intended for production.
if (env['process.env'].NODE_ENV !== '"production"') {
  throw new Error('Production builds must have NODE_ENV=production.');
}

module.exports = {
  devtool: 'source-map',
  entry: {
    'zebulon-grid': './src/pivotGrid/index.js',
  },
  output: {
    path: 'lib/umd',
    filename: '[name].js',
    libraryTarget: 'umd',
    library: 'ZebulonGrid',
  },
  externals: {
    'react': 'React',
    'react-dom': 'ReactDOM',
  },
  plugins: [
    new webpack.optimize.UglifyJsPlugin({
      beautify: true,
      comments: true,
      mangle: false,
    }),
  ],
  resolve: {
    extensions: ['', '.js', '.jsx'],
  },
  module: {
    loaders: [
      {
        test: /\.jsx?$/,
        loader: 'babel',
        include: path.join(__dirname, 'src/pivotGrid'),
        query: {
          presets: ['react-app'],
        },
      },
    ],
  },
};
