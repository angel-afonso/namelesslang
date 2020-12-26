import Controller from '../controller';
import Output from '../output';
import Editor from '../editor';
import styles from './styles.css';
import {useRef} from 'react';

export default function App() {
	const code = useRef("");

	function setCode(value) {
		code.current = code;
	}

	return (
		<div>
			<Controller />
			<div className={styles.editorContainer}>
				<Editor setCode={setCode} />
				<Output output="sadad" />
			</div>
		</div>
	)
}
