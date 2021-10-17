import { faGripVertical, faPlay } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { DragEvent, useRef } from "react";
import style from "./Controls.module.css";

export default function Controls(): JSX.Element {
  const ref = useRef<HTMLDivElement>();

  const onDrag = (e: DragEvent<HTMLDivElement>) => {
    const img = new Image();
    img.src =
      "data:image/gif;base64,R0lGODlhAQABAIAAAAUEBAAAACwAAAAAAQABAAACAkQBADs=";
    e.dataTransfer.setDragImage(img, 0, 0);

    const element = ref.current as HTMLDivElement;

    console.log(`calc(${e.clientX}px + ${element.style.width}px)`);

    element.style.top = `${e.clientY}px`;
    element.style.left = `calc(${e.clientX}px + ${element.style.width}px)`;
  };

  const onDragEnd = (e: DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    e.stopPropagation();

    const element = ref.current as HTMLDivElement;

    element.style.top = e.clientY.toString() + "px";
    element.style.left = e.clientX.toString() + "px";
  };

  return (
    <div ref={ref} className={style.container}>
      <div className={style.icon}>
        <FontAwesomeIcon icon={faPlay} color="green" />
      </div>
      <div
        className={style.icon}
        draggable
        onDrag={onDrag}
        onDragEnd={onDragEnd}
      >
        <FontAwesomeIcon icon={faGripVertical} color="gray" />
      </div>
    </div>
  );
}
