import { useEffect, useState } from "react";
import { Row, Col, Container } from "react-bootstrap";
import "bootstrap/dist/css/bootstrap.min.css";
import Navbar from "./components/Navbar";

import CodeMirror from "@uiw/react-codemirror";
import Results from "./components/Results";

export type Snippet = { title: string; code: string };

type Result = { ast: string; output: string };

function App() {
  const [code, setCode] = useState<string>("");
  const [result, setSteps] = useState<Array<Result>>([]);
  const [env, setEnv] = useState({});
  const [traceLog, setTraceLog] = useState([]);
  const [error, setError] = useState<string>("");
  const [snippets, setSnippets] = useState<Array<Snippet>>([]);

  useEffect(() => {
    const snips = window.localStorage.getItem("snippets");
    if (!snips) {
      window.localStorage.setItem("snippets", JSON.stringify([]));
    }
    setSnippets(JSON.parse(snips));
  }, []);

  const evaluateCode = async () => {
    // Ensure 'code', 'setEnv', 'setSteps', 'setError', 'setTraceLog'
    // are defined and accessible in this scope (e.g., from React state and props).

    const controller = new AbortController();
    const signal = controller.signal;

    // Set a 5-second timeout. If it fires, it will abort the fetch request.
    const timeoutId = setTimeout(() => {
      controller.abort();
    }, 5000); // 5000 milliseconds = 5 seconds

    try {
      const res = await fetch("http://localhost:3000/evaluate", {
        method: "post",
        body: code, // 'code' should be the string input for your L language
        signal: signal, // Pass the abort signal to fetch
        headers: {
          // Specify content type as plain text, as the Haskell server decodes raw UTF-8
          'Content-Type': 'text/plain;charset=UTF-8'
        }
      });

      // If the fetch request completes (either successfully or with an HTTP error status)
      // before the timeout, clear the timeout.
      clearTimeout(timeoutId);

      if (!res.ok) {
        let errorMsg = `Server error: ${res.status} ${res.statusText}`;
        try {
          // Try to get more details from the response body if available
          const errorBody = await res.text();
          if (errorBody) {
            errorMsg += ` - ${errorBody}`;
          }
        } catch (e) {
          // Ignore error while reading error body
        }
        setError(errorMsg);
        // Reset other relevant states
        setEnv({}); // Or initial/default environment
        setSteps([]);
        setTraceLog([]);
        return;
      }

      // If res.ok is true, expect a JSON response as defined by your Haskell server
      const responseData = await res.json();
      // responseData should be like: { finalEnvironment, steps, finalError, traceLog }

      setEnv(responseData.finalEnvironment);
      setSteps(responseData.steps);
      setError(responseData.finalError); // This shows errors from the server logic
      setTraceLog(responseData.traceLog);

    } catch (error) {
      // This catch block handles:
      // 1. AbortError: If controller.abort() was called due to our client-side timeout.
      // 2. Network errors: If the server is unreachable, DNS issues, etc.
      // 3. JSON parsing errors: If res.json() fails.

      clearTimeout(timeoutId); // Important to clear timeout in case of other errors before timeout fires

      if (error.name === 'AbortError') {
        // Specifically handle the timeout abort
        setError("Request timed out: The server did not respond within 5 seconds.");
      } else {
        // Handle other fetch-related errors (network issues, JSON parsing failure, etc.)
        console.error('Fetch operation failed:', error);
        setError("Failed to communicate with the server or process the response. Please check your network and try again.");
      }

      // Reset other relevant states to a default or empty state on any error
      setEnv({}); // Or your initial/default environment
      setSteps([]);
      setTraceLog([]);
    }
  };
  const handleClearState = () => {
    setCode("");
    setEnv({});
    setSteps([]);
    setError("");
  };

  const handleEditorChange = (value: string) => {
    setCode(value);
  };

  const handleSaveSnippet = (name?: string) => {
    let newSnippets;
    const existingSnippet = snippets.find((s) => s.title == name);
    if (!existingSnippet) {
      newSnippets = [
        ...snippets,
        { title: name ? name : String(Date.now()), code },
      ];
    } else {
      const oldSnippets = snippets.filter(
        (s) => s.title !== existingSnippet.title,
      );
      console.log({ oldSnippets });
      newSnippets = [...oldSnippets, { title: existingSnippet.title, code }];
    }
    window.localStorage.setItem("snippets", JSON.stringify(newSnippets));
    setSnippets(newSnippets);
  };

  const handleSelectSnippet = (title: string) => {
    const foundSnippet: Snippet | undefined = snippets.find(
      (s) => s.title === title,
    );
    if (foundSnippet) setCode(foundSnippet.code);
  };

  const handleAddNewSnippet = () => {
    const newSnip = { title: Date.now(), code: "" };
    const newSnippets = [...snippets, newSnip];
    setCode(newSnip.code);
    setSnippets(newSnippets);
  };

  return (
    <main
      style={{
        width: "100vw",
        height: "100vh",
        overflow: "hidden",
      }}
    >
      <Navbar
        snippets={snippets}
        currentSnippet={code}
        handleSelectSnippet={handleSelectSnippet}
        evaluateCode={evaluateCode}
        handleAddNewSnippet={handleAddNewSnippet}
        handleSaveSnippet={handleSaveSnippet}
        handleClearState={handleClearState}
      />
      <Row>
        <Col
          md={7}
          style={{
            paddingRight: 0,
          }}
        >
          <CodeMirror
            lang="haskell"
            value={code}
            height="95vh"
            extensions={[]}
            onChange={handleEditorChange}
          />
        </Col>
        <Col
          md={5}
          style={{
            padding: 0,
          }}
        >
          <Results
            env={env}
            result={result}
            error={error}
            traceLog={traceLog}
          />
        </Col>
        {/* <Col md={6}> */}
        {/*   <ul> */}
        {/*     {snippets.map(snip => <li key={snip.id} onClick={e => handleSelectSnippet(snip.id)}>{snip.id}</li>)} */}
        {/*   </ul> */}
        {/* </Col> */}
      </Row>
    </main>
  );
}
export default App;
