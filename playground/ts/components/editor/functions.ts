import {ClipboardEvent, KeyboardEvent} from "react";

function insertText(target: HTMLDivElement, text: string) {
	const doc = target.ownerDocument.defaultView;
	const sel = doc.getSelection();
	const range = sel.getRangeAt(0);

	const tabNode = document.createTextNode(text);
	range.insertNode(tabNode);

	range.setStartAfter(tabNode);
	range.setEndAfter(tabNode);
	sel.removeAllRanges();
	sel.addRange(range);
}

function mapKeyToText(event: KeyboardEvent<HTMLDivElement>) {
	const target = event.target as HTMLDivElement;
	switch (event.key) {
		case 'Tab':
			event.preventDefault();
			insertText(target, '\u00a0\u00a0\u00a0\u00a0')
			break;
		default:
			break;
	}
}

export function useCode(setCode: Function): Array<any> {

	function onType(event: KeyboardEvent<HTMLDivElement>) {
		mapKeyToText(event);
		setCode((event.target as HTMLDivElement).textContent);
	}

	function onPaste(e: ClipboardEvent<HTMLDivElement>) {
		const target = e.target as HTMLDivElement;
		setCode(target.textContent);
	}

	return [onType, onPaste];
}
