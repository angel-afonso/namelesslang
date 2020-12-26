import {ChangeEvent, useState} from 'react';
import styles from './styles.css';

interface Props {
	setCode: Function
}

export default function Editor({setCode}: Props) {
	const [value, setValue] = useState("");

	function onChange(e: ChangeEvent<HTMLTextAreaElement>) {
		setValue(e.target.value);
		setCode(e.target.value);
	}

	return (
		<div className={styles.textarea}>
			<textarea value={value} onChange={onChange} />
		</div>
	);
}
