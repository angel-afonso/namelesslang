import ReactDOM from 'react-dom';
import App from './components/app/app';

ReactDOM.render(<App />, document.getElementById('root'));

declare global {
	interface NodeModule {hot: {accept: Function, dispose: Function}}
}

if (module.hot) {
	module.hot.accept();
}

