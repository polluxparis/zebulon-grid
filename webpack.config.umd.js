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
    react: 'React',
    'react-dom': 'ReactDOM',
  },
  plugins: [
    // Minify the code.
    new webpack.optimize.UglifyJsPlugin({
      compress: {
        screw_ie8: true, // React doesn't support IE8
        warnings: false,
      },
      mangle: {
        screw_ie8: true,
      },
      output: {
        comments: false,
        screw_ie8: true,
      },
      sourceMap: true,
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
