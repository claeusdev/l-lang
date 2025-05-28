import { useState, type JSX } from 'react';
import { Form } from 'react-bootstrap';

const Pre = ({ children }) => {
  return <pre className="p-3 overflow-auto" style={{ minHeight: "90vh" }}>
    {children}
  </pre>
}

type ResultMenuItem = {
  title: string,
  content: (opts: ResultsProps) => JSX.Element
}
const resultsMenuItems: Array<ResultMenuItem> = [
  {
    title: "Output", content: ({ result, error }: ResultsProps) =>
      <Pre>
        {!result && <p>"Click on evaluate to run the code on the left"</p>}
        {result && result.map(r => <p key={r.ast}>{r.output}</p>)}
        {error && error}
      </Pre>
  },
  {
    title: "Ast", content: ({ result }: ResultsProps) =>
      <Pre>
        {result.map(r => <p key={r.ast}>{r.ast}</p>)}
      </Pre>
  },
  {
    title: "Eval Steps", content: ({ traceLog }: ResultsProps) =>
      <Pre>
        {traceLog.map(trace => <p>&gt; {trace}</p>)}
      </Pre>
  },
  {
    title: "Environment", content: ({ env }: ResultsProps) =>
      <Pre>
        {JSON.stringify(env)}
      </Pre>
  },
]

type ResultsProps = {
  result: Array<{ ast: string, output: string }>,
  env: Record<string, string>,
  traceLog: Array<String>,
  error: string
}

function Results({ result, error, env, traceLog }: ResultsProps) {
  const [contentToShow, setContentToShow] = useState<ResultMenuItem>(resultsMenuItems[0])

  const handleContentChange = (title: string) => {
    const cnt = resultsMenuItems.find(r => r.title === title);
    if (cnt) {
      setContentToShow(cnt)
    }
  }

  return (
    <section className='bg-light border ' style={{
      minHeight: "100%"
    }}>
      <Form.Select aria-label="Default select example" onChange={e => handleContentChange(e.target.value)}>
        {resultsMenuItems.map(res => <option key={res.title} value={res.title}>{res.title}</option>)}
      </Form.Select>
      <div>
        {contentToShow.content({ result, error, env, traceLog })}
      </div>
    </section>
  );
}

export default Results;
