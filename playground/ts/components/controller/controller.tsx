import {ReactComponent as Run} from '../../assets/run.svg';
import styles from './styles.module.css';

export default function Controller() {
	return (
		<div>
			<button className={styles.runButton}>
				<Run />
			</button>
		</div>
	);
}
