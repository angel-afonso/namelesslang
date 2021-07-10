import { useEffect } from "react";
import { editor } from "monaco-editor";

export default function Editor(): JSX.Element {
  useEffect(() => {
    editor.create(document.getElementById("editor")!, {
      value: ["function x() {", '\tconsole.log("Hello world!");', "}"].join(
        "\n"
      ),
      language: "javascript",
      theme: "vs-dark",
      minimap: { enabled: true },
    });
  });

  return <div id="editor" style={{ height: "100vh" }} />;
}
