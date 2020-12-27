import {useEffect, useRef, useState} from 'react';
import Controller from '../controller';
import Output from '../output';
import Editor from '../editor';
import styles from './styles.css';

export default function App() {
	const code = useRef("");
	const wasm = useRef(null);
	const [output, setOutput] = useState("");

	useEffect(() => {
		import('../../../pkg').then(wsm => wasm.current = wsm);
	}, []);

	function setCode(value: string) {
		code.current = value;
	}

	function run() {
		setOutput("");
		wasm.current.run_code(code.current, (out: string) => setOutput(output => (output + out).replace('\n', '<br/>')));
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
