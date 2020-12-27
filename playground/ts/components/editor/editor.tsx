import {useCode} from './functions';
import styles from './styles.css';

interface Props {
	setCode: Function
}

export default function Editor({setCode}: Props) {
	const [onType, onPaste] = useCode(setCode);

	return (
		<div className={styles.codeConainer}>
			<div className={styles.code} spellCheck={false} tabIndex={0} onPaste={onPaste} contentEditable onKeyDown={onType} />
		</div>
	);
}
