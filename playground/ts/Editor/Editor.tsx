import { useEffect } from "react";
import { editor } from "monaco-editor";
import Controls from "../Controls";

import styles from "./Editor.module.css";

export default function Editor(): JSX.Element {
  useEffect(() => {
    editor.create(document.getElementById("editor")!, {
      value: ['println("hello world");'].join("\n"),
      language: "nameless",
      theme: "vs-dark",
      minimap: { enabled: true },
      padding: { top: 12 },
    });
  });

  return (
    <div style={{ display: "flex" }}>
      <Controls />
      <div id="editor" className={styles.editor} />
      <div className={styles.output}>a</div>
    </div>
  );
}
