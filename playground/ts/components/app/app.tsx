import {useEffect, useRef, useState} from 'react';
import Controller from '../controller';
import Output from '../output';
import Editor from '../editor';
import styles from './styles.css';

export default function App() {
	const [output, setOutput] = useState("");
	const code = useRef("");
	const wasm = useRef(null);

	useEffect(() => {
		import('../../../pkg').then(wsm => wasm.current = wsm);
	}, []);

	function setCode(value: string) {
		code.current = value;
	}

	function run() {
		setOutput("");
		const start = Date.now();

		wasm.current.run_code(code.current, (out: string) => setOutput((output) => output + '<br/>' + out))
			.then(() => setOutput((output) => output + '<br/><br/>----------------------<br/>Execution time: ' +
				(Date.now() - start) / 1000 + 'seg'
			))
			.catch((error: string) => setOutput(error));
	}

	return (
		<div>
			<Controller onRun={run} />
			<div className={styles.editorContainer}>
				<Editor setCode={setCode} />
				<Output output={output} />
			</div>
		</div>
	)
}
