import styles from './styles.css';

interface Props {
	output?: string,
}

export default function Output({output}: Props) {
	return (
		<div id="output" className={styles.output} dangerouslySetInnerHTML={{__html: output}} />
	)
}
