interface Props {
	output?: string,
}

export default function Output({output}: Props) {
	return (
		<div>
			{output}
		</div>
	)
}
