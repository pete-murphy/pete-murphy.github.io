import {
  createElement as h,
  render,
  Component,
} from "https://unpkg.com/preact@latest?module";
import {
  useEffect,
  useMemo,
  useState,
} from "https://unpkg.com/preact@latest/hooks/dist/hooks.module.js?module";
import { motion } from "https://unpkg.com/framer-motion@latest?module";
import htm from "https://unpkg.com/htm?module";

// Initialize htm with Preact
const html = htm.bind(h);

const App = () => {
  const size = 5;
  const [matrix, setMatrix] = useState(makeMatrix(size));
  const [indices, setIndices] = useState < ReadonlyArray < Index >> [];
  const targetMatrix = useMemo(
    () => makeTargetMatrix(makeMatrix(size)),
    [size]
  );
  useEffect(() => {
    const listener = (e) => {
      if (e.key === "Enter") {
        e.preventDefault();
        setMatrix(
          swapIndices([indices[0], indices[1], indices[2], indices[3]])
        );
      }
      if (e.key === "Escape") {
        setIndices([]);
      }
    };
    document.addEventListener("keydown", listener);
    return () => document.removeEventListener("keydown", listener);
  }, [indices]);

  return html`<div className="container">
    ${matrix.map((row, rowIndex) =>
      row.map(
        (cell, columnIndex) =>
          html`<button
            key="${rowIndex} ${columnIndex}"
            className="cell"
            onMouseDown="${() =>
              setIndices(toggleIndex([rowIndex, columnIndex]))}"
            onMouseOver="${(e) => {
              if (e.buttons === 1) {
                setIndices(toggleIndex([rowIndex, columnIndex]));
              }
            }}"
          >
            <${motion.div}
              key="${cell}"
              layoutId="${cell.toString()}"
              className="${[
                "cell-inner",
                indices.some(([r, c]) => r === rowIndex && c === columnIndex)
                  ? "selected"
                  : "",
                cell === targetMatrix[rowIndex][columnIndex] ? "correct" : "",
              ].join(" ")}"
              transition="${{ duration: 1.5 }}"
            >
              ${cell}
            </${motion.div}>
          </button>`
      )
    )}
  </div> `;
  // return html`<div>
  //   <h1>Test ${props.name}!: ${countVariable}</h1>
  //   <button onClick=${logMethod}>Increment</button>
  // </div>`;
};

render(html`<${App} />`, document.getElementById("app"));
