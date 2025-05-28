import React, { useState } from "react";
import { Button, Dropdown, Form, InputGroup } from "react-bootstrap";
import type { Snippet } from "../App";

interface Props {
  evaluateCode: () => void;
  handleSaveSnippet: (name: string) => void;
  handleAddNewSnippet: () => void;
  handleClearState: () => void;
  handleSelectSnippet: (title: string) => void;
  snippets: Array<Snippet>;
  currentSnippet: Snippet;
}
export default function Navbar({
  currentSnippet,
  handleSelectSnippet,
  evaluateCode,
  handleSaveSnippet,
  handleClearState,
  snippets,
}: Props) {
  const [name, setName] = useState<string>("");

  const handleSaveCodeSnippet = (e) => {
    e.preventDefault();
    handleSaveSnippet(name);
    alert("Snippet successfully saved!");
    setName("");
  };

  return (
    <nav className="navbar navbar-expand-lg bg-body-tertiary">
      <a className="navbar-brand ml-2" href="#">
        L Lang
      </a>
      <div className="d-flex gap-2">
        <Button variant="primary" className="mr-2" onClick={evaluateCode}>
          Evaluate
        </Button>
        <Dropdown>
          <Dropdown.Toggle
            variant="success"
            id="dropdown-basic"
            disabled={!currentSnippet}
          >
            Save Snippet
          </Dropdown.Toggle>

          <Dropdown.Menu
            style={{
              minWidth: 220,
            }}
          >
            <Form onSubmit={handleSaveCodeSnippet}>
              <InputGroup
                className="mb-3 mx-2"
                style={{
                  width: "90%",
                }}
              >
                <Form.Control
                  placeholder="Name"
                  aria-label="Name"
                  aria-describedby="basic-addon2"
                  value={name}
                  onChange={(e) => setName(e.target.value)}
                />
                <Button variant="success" id="button-addon2" type="submit">
                  Save
                </Button>
              </InputGroup>
            </Form>
          </Dropdown.Menu>
        </Dropdown>
        <Button variant="danger" onClick={handleClearState}>
          Clear
        </Button>
        <Dropdown>
          <Dropdown.Toggle variant="success" id="dropdown-basic">
            Saved Snippets
          </Dropdown.Toggle>

          <Dropdown.Menu>
            {snippets.map((snippet) => (
              <Dropdown.Item
                key={snippet.title}
                onClick={(_) => handleSelectSnippet(snippet.title)}
              >
                {snippet.title}
              </Dropdown.Item>
            ))}
          </Dropdown.Menu>
        </Dropdown>
      </div>
    </nav>
  );
}
