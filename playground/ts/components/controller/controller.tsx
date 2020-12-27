import styles from './styles.css';

interface Props {
	onRun: Function
}

export default function Controller({onRun}: Props) {
	return (
		<div className={styles.controller}>
			<button className={styles.runButton} onClick={(_) => onRun()}>
				Run
			</button>
		</div>
	);
}
