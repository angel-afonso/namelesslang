const path = require("path");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const FriendlyErrorsWebpackPlugin = require('friendly-errors-webpack-plugin');

const PORT = process.env.PORT || 3000;

const dist = path.resolve(__dirname, "dist");

module.exports = {
	mode: "production",
	entry: "./ts/index.tsx",
	output: {
		path: dist,
		filename: "bundle.[hash].js"
	},
	devServer: {
		contentBase: dist,
		port: PORT,
		hot: true,
		quiet: true,
	},
	devtool: 'source-map',
	module: {
		rules: [
			{
				test: /.(js|ts)x?$/,
				exclude: /node_modules/,
				use: {
					loader: 'babel-loader',
				},
			},
			{
				test: /\.css$/,
				use: [
					{
						loader: 'style-loader',
						options: {injectType: 'singletonStyleTag'},
					},
					'css-loader',
				],
			},
			{
				test: /\.svg$/,
				use: ['@svgr/webpack', 'url-loader'],
			},
		],
	},
	resolve: {
		extensions: ['.ts', '.tsx', '.js', 'jsx']
	},
	plugins: [
		new FriendlyErrorsWebpackPlugin(),
		new HtmlWebpackPlugin({
			template: 'public/index.html'
		}),
		new WasmPackPlugin({
			crateDirectory: __dirname,
		}),
	]
};
