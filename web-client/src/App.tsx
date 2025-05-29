import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { Card, CardContent } from "@/components/ui/card";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Textarea } from "@/components/ui/textarea";
import {
  AlertCircle,
  CheckCircle,
  Copy,
  Download,
  FolderOpen,
  Info,
  Play,
  Plus,
  Save,
  Settings,
  Share,
  Trash2,
} from "lucide-react";
import { useEffect, useState } from "react";
import { Toaster, toast } from "sonner";

export type Snippet = { title: string; code: string };

type Result = { ast: string; output: string };

interface LintError {
  type: "error" | "warning";
  message: string;
  line?: number;
}

export default function App() {
  const [code, setCode] = useState<string>("");
  const [result, setSteps] = useState<Array<Result>>([]);
  const [env, setEnv] = useState({});
  const [error, setError] = useState<string>("");
  const [snippets, setSnippets] = useState<Array<Snippet>>([]);
  const [isEvaluating, setIsEvaluating] = useState(false);
  const [errors, setErrors] = useState<Array<LintError>>([]);
  const [showSnippets, setShowSnippets] = useState(false);

  useEffect(() => {
    const urlParams = new URLSearchParams(window.location.search);
    const encodedCode = urlParams.get("code");

    if (encodedCode) {
      try {
        const decodedCode = atob(encodedCode);
        setCode(decodedCode);
        toast.success("Code loaded from share link!", {
          description: "Successfully loaded shared code",
          duration: 3000,
        });

        window.history.replaceState(null, "", window.location.pathname);
      } catch {
        toast.error("Failed to load shared code", {
          description: "The share link appears to be invalid",
          duration: 3000,
        });
      }
    }
  }, []);

  useEffect(() => {
    const snips = window.localStorage.getItem("snippets");
    if (!snips) {
      window.localStorage.setItem("snippets", JSON.stringify([]));
      setSnippets([]);
    } else {
      setSnippets(JSON.parse(snips));
    }
  }, []);

  useEffect(() => {
    const newErrors: LintError[] = [];
    if (error) {
      newErrors.push({
        type: "error",
        message: error,
      });
    }
    setErrors(newErrors);
  }, [error]);

  const evaluateCode = async () => {
    if (!code.trim()) {
      toast.warning("No code to evaluate", {
        description: "Please write some code before running",
        duration: 2000,
      });
      return;
    }

    setIsEvaluating(true);
    setError("");

    toast.loading("Evaluating code...", {
      id: "evaluation",
      description: "Sending code to L language server",
    });

    const controller = new AbortController();
    const signal = controller.signal;

    const timeoutId = setTimeout(() => {
      controller.abort();
    }, 5000);

    try {
      const res = await fetch("http://localhost:3000/evaluate", {
        method: "post",
        body: code,
        signal: signal,
        headers: {
          "Content-Type": "text/plain;charset=UTF-8",
        },
      });

      clearTimeout(timeoutId);

      if (!res.ok) {
        let errorMsg = `Server error: ${res.status} ${res.statusText}`;
        try {
          const errorBody = await res.text();
          if (errorBody) {
            errorMsg = `${errorMsg} - ${errorBody}`;
          }
        } catch {
          // Ignore error while reading error body
        }
        setError(errorMsg);
        setEnv({});
        setSteps([]);

        toast.error("Evaluation failed", {
          id: "evaluation",
          description: errorMsg,
          duration: 4000,
        });
        return;
      }

      const responseData = await res.json();

      setEnv(responseData.finalEnvironment);
      setSteps(responseData.steps);
      setError(responseData.finalError);

      if (responseData.finalError) {
        toast.error("Evaluation completed with errors", {
          id: "evaluation",
          description: responseData.finalError,
          duration: 4000,
        });
      } else {
        toast.success("Code evaluated successfully!", {
          id: "evaluation",
          description: `Generated ${responseData.steps?.length || 0} steps`,
          duration: 3000,
        });
      }
    } catch (err) {
      clearTimeout(timeoutId);

      if (err instanceof Error && err.name === "AbortError") {
        const timeoutMsg =
          "Request timed out: The server did not respond within 5 seconds.";
        setError(timeoutMsg);
        toast.error("Evaluation timed out", {
          id: "evaluation",
          description: timeoutMsg,
          duration: 4000,
        });
      } else {
        console.error("Fetch operation failed:", err);
        const networkMsg =
          "Failed to communicate with the server or process the response. Please check your network and try again.";
        setError(networkMsg);
        toast.error("Network error", {
          id: "evaluation",
          description: networkMsg,
          duration: 4000,
        });
      }

      setEnv({});
      setSteps([]);
    } finally {
      setIsEvaluating(false);
    }
  };

  const handleSaveSnippet = (name?: string) => {
    if (!code.trim()) {
      toast.warning("No code to save", {
        description: "Please write some code before saving",
        duration: 2000,
      });
      return;
    }

    let newSnippets: Snippet[];
    const snippetName = name ? name : String(Date.now());
    const existingSnippet = snippets.find((s) => s.title === snippetName);

    if (!existingSnippet) {
      newSnippets = [...snippets, { title: snippetName, code }];
    } else {
      const oldSnippets = snippets.filter(
        (s) => s.title !== existingSnippet.title
      );
      newSnippets = [...oldSnippets, { title: existingSnippet.title, code }];
    }

    window.localStorage.setItem("snippets", JSON.stringify(newSnippets));
    setSnippets(newSnippets);

    toast.success("Snippet saved!", {
      description: `Saved as "${snippetName}"`,
      duration: 2000,
    });
  };

  const handleSelectSnippet = (title: string) => {
    const foundSnippet: Snippet | undefined = snippets.find(
      (s) => s.title === title
    );
    if (foundSnippet) {
      setCode(foundSnippet.code);
      setShowSnippets(false);
      toast.success("Snippet loaded!", {
        description: `Loaded "${title}"`,
        duration: 2000,
      });
    }
  };

  const handleAddNewSnippet = () => {
    const newSnip: Snippet = { title: String(Date.now()), code: "" };
    const newSnippets = [...snippets, newSnip];
    setCode(newSnip.code);
    setSnippets(newSnippets);
    window.localStorage.setItem("snippets", JSON.stringify(newSnippets));
    setShowSnippets(false);

    toast.success("New snippet created!", {
      description: "Start coding in your new snippet",
      duration: 2000,
    });
  };

  const handleDeleteSnippet = (title: string) => {
    const newSnippets = snippets.filter((s) => s.title !== title);
    setSnippets(newSnippets);
    window.localStorage.setItem("snippets", JSON.stringify(newSnippets));

    toast.success("Snippet deleted!", {
      description: `"${title}" has been removed`,
      duration: 2000,
    });
  };

  const handleShare = async () => {
    if (!code.trim()) {
      toast.warning("No code to share", {
        description: "Please write some code before sharing",
        duration: 2000,
      });
      return;
    }

    try {
      const encoded = btoa(code);
      const url = `${window.location.origin}${window.location.pathname}?code=${encoded}`;
      await navigator.clipboard.writeText(url);

      toast.success("Share link copied!", {
        description: "Share link has been copied to clipboard",
        duration: 3000,
      });
    } catch {
      toast.error("Failed to copy share link", {
        description: "Please try again",
        duration: 3000,
      });
    }
  };

  const handleCopy = async () => {
    if (!code.trim()) {
      toast.warning("No code to copy", {
        description: "Please write some code before copying",
        duration: 2000,
      });
      return;
    }

    try {
      await navigator.clipboard.writeText(code);
      toast.success("Code copied!", {
        description: "Code has been copied to clipboard",
        duration: 2000,
      });
    } catch {
      toast.error("Failed to copy code", {
        description: "Please try again",
        duration: 3000,
      });
    }
  };

  const handleDownload = () => {
    if (!code.trim()) {
      toast.warning("No code to download", {
        description: "Please write some code before downloading",
        duration: 2000,
      });
      return;
    }

    try {
      const blob = new Blob([code], { type: "text/plain" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = "program.l";
      a.click();
      URL.revokeObjectURL(url);

      toast.success("File downloaded!", {
        description: "program.l has been downloaded",
        duration: 2000,
      });
    } catch {
      toast.error("Failed to download file", {
        description: "Please try again",
        duration: 3000,
      });
    }
  };

  const formatOutput = () => {
    if (result && result.length > 0) {
      const lastStep = result[result.length - 1];
      return lastStep.output || "No output generated";
    }
    return "Run your code to see the output...";
  };

  const formatEvalSteps = () => {
    if (result && result.length > 0) {
      let output = "> Evaluation steps:\n\n";
      result.forEach((step, index) => {
        output = `${output}Step ${index + 1}:\n`;
        output = `${output}AST: ${step.ast}\n`;
        output = `${output}Output: ${step.output}\n\n`;
      });
      return output;
    }
    return "No evaluation steps available";
  };

  const formatEnvironment = () => {
    if (env && Object.keys(env).length > 0) {
      return JSON.stringify(env, null, 2);
    }
    return "No environment data available";
  };

  const formatCompiledOutput = () => {
    if (result && result.length > 0) {
      return result.map((step) => step.ast).join("\n\n");
    }
    return "Compiled AST will appear here...";
  };

  return (
    <div className="min-h-screen bg-gray-50 flex flex-col">
      <Toaster position="bottom-right" richColors />

      <div className="bg-white border-b border-gray-200 px-4 py-3">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-4">
            <h1 className="text-xl font-semibold text-gray-900">
              L Language Playground
            </h1>
            <div className="flex items-center gap-2">
              <Badge
                variant={
                  isEvaluating
                    ? "secondary"
                    : errors?.some((e) => e.type === "error")
                    ? "destructive"
                    : "default"
                }
              >
                {isEvaluating
                  ? "Evaluating..."
                  : errors?.some((e) => e.type === "error")
                  ? "Error"
                  : "Ready"}
              </Badge>
              {!isEvaluating && errors && errors.length === 0 && (
                <CheckCircle className="w-4 h-4 text-green-500" />
              )}
            </div>
          </div>

          <div className="flex items-center gap-2">
            <Button
              variant="outline"
              size="sm"
              onClick={evaluateCode}
              disabled={isEvaluating}
            >
              <Play className="w-4 h-4 mr-1" />
              Run
            </Button>
            <div className="relative">
              <Button
                variant="outline"
                size="sm"
                onClick={() => {
                  setShowSnippets(!showSnippets);
                  if (!showSnippets) {
                    toast("Opening snippets", {
                      description: "Manage your saved code snippets",
                      duration: 1500,
                    });
                  }
                }}
              >
                <FolderOpen className="w-4 h-4 mr-1" />
                Snippets
              </Button>
              {showSnippets && (
                <div className="absolute top-full right-0 mt-1 w-64 bg-white border border-gray-200 rounded-md shadow-lg z-10">
                  <div className="p-2">
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={handleAddNewSnippet}
                      className="w-full mb-2"
                    >
                      <Plus className="w-4 h-4 mr-2" />
                      New Snippet
                    </Button>
                    {snippets.length > 0 && (
                      <div className="border-t pt-2">
                        {snippets.map((snippet) => (
                          <div
                            key={snippet.title}
                            className="flex items-center justify-between p-2 hover:bg-gray-50 rounded"
                          >
                            <button
                              type="button"
                              onClick={() => handleSelectSnippet(snippet.title)}
                              className="flex-1 cursor-pointer text-sm text-left"
                            >
                              {snippet.title}
                            </button>
                            <Button
                              variant="ghost"
                              size="sm"
                              onClick={() => handleDeleteSnippet(snippet.title)}
                            >
                              <Trash2 className="w-3 h-3" />
                            </Button>
                          </div>
                        ))}
                      </div>
                    )}
                  </div>
                </div>
              )}
            </div>
            <Button
              variant="outline"
              size="sm"
              onClick={() => handleSaveSnippet(String(Date.now()))}
            >
              <Save className="w-4 h-4 mr-1" />
              Save
            </Button>
            <Button variant="outline" size="sm" onClick={handleCopy}>
              <Copy className="w-4 h-4 mr-1" />
              Copy
            </Button>
            <Button variant="outline" size="sm" onClick={handleShare}>
              <Share className="w-4 h-4 mr-1" />
              Share
            </Button>
            <Button variant="outline" size="sm" onClick={handleDownload}>
              <Download className="w-4 h-4 mr-1" />
              Download
            </Button>
          </div>
        </div>
      </div>

      <div className="flex-1 flex">
        <div className="w-1/2 border-r border-gray-200 bg-white flex flex-col">
          <div className="border-b border-gray-200 px-4 py-2 bg-gray-50">
            <div className="flex items-center justify-between">
              <span className="text-sm font-medium text-gray-700">Editor</span>
              <div className="text-xs text-gray-500">
                Lines: {code.split("\n").length} | Chars: {code.length}
              </div>
            </div>
          </div>

          <div className="flex-1 p-4">
            <Textarea
              value={code}
              onChange={(e) => setCode(e.target.value)}
              className="w-full h-full resize-none border-0 rounded-none outline-none shadow-none focus-visible:ring-0 font-mono text-sm"
              style={{
                fontFamily: 'Monaco, Menlo, "Ubuntu Mono", monospace',
                minHeight: "500px",
              }}
              placeholder="Write your L language code here..."
            />
          </div>
        </div>

        <div className="w-1/2 bg-white flex flex-col">
          <Tabs defaultValue="output" className="flex-1 flex flex-col">
            <div className="border-b border-gray-200 px-4 py-2 bg-gray-50">
              <TabsList className="grid w-full grid-cols-5">
                <TabsTrigger value="output" className="text-xs">
                  <Play className="w-3 h-3 mr-1" />
                  Output
                </TabsTrigger>
                <TabsTrigger value="ast" className="text-xs">
                  <Info className="w-3 h-3 mr-1" />
                  AST
                </TabsTrigger>
                <TabsTrigger value="evalsteps" className="text-xs">
                  <CheckCircle className="w-3 h-3 mr-1" />
                  Eval Steps
                </TabsTrigger>
                <TabsTrigger value="environment" className="text-xs">
                  <Settings className="w-3 h-3 mr-1" />
                  Environment
                </TabsTrigger>
                <TabsTrigger value="issues" className="text-xs">
                  <AlertCircle className="w-3 h-3 mr-1" />
                  Issues ({errors?.length || 0})
                </TabsTrigger>
              </TabsList>
            </div>

            <div className="flex-1 overflow-hidden">
              <TabsContent value="output" className="h-full m-0">
                <div className="h-full p-4">
                  <div className="bg-gray-900 text-green-400 p-4 rounded-md h-full overflow-auto font-mono text-sm">
                    <pre className="whitespace-pre-wrap">{formatOutput()}</pre>
                  </div>
                </div>
              </TabsContent>

              <TabsContent value="ast" className="h-full m-0">
                <div className="h-full p-4">
                  <div className="bg-gray-50 border rounded-md h-full overflow-auto">
                    <pre className="p-4 text-sm font-mono whitespace-pre-wrap">
                      {formatCompiledOutput()}
                    </pre>
                  </div>
                </div>
              </TabsContent>

              <TabsContent value="evalsteps" className="h-full m-0">
                <div className="h-full p-4">
                  <div className="bg-gray-50 border rounded-md h-full overflow-auto">
                    <pre className="p-4 text-sm font-mono whitespace-pre-wrap">
                      {formatEvalSteps()}
                    </pre>
                  </div>
                </div>
              </TabsContent>

              <TabsContent value="environment" className="h-full m-0">
                <div className="h-full p-4">
                  <div className="bg-gray-50 border rounded-md h-full overflow-auto">
                    <pre className="p-4 text-sm font-mono whitespace-pre-wrap">
                      {formatEnvironment()}
                    </pre>
                  </div>
                </div>
              </TabsContent>

              <TabsContent value="issues" className="h-full m-0">
                <div className="h-full p-4">
                  {!errors || errors.length === 0 ? (
                    <div className="flex items-center justify-center h-full text-gray-500">
                      <div className="text-center">
                        <CheckCircle className="w-12 h-12 mx-auto mb-2 text-green-500" />
                        <p>No issues found</p>
                      </div>
                    </div>
                  ) : (
                    <div className="space-y-2">
                      {errors.map((error, index) => (
                        <Card
                          key={`error-${error.message}-${index}`}
                          className={`border-l-4 p-0 ${
                            error.type === "error"
                              ? "border-l-red-500"
                              : "border-l-yellow-500"
                          }`}
                        >
                          <CardContent className="p-3">
                            <div className="flex items-start gap-2">
                              {error.type === "error" ? (
                                <AlertCircle className="w-4 h-4 text-red-500 mt-0.5" />
                              ) : (
                                <Info className="w-4 h-4 text-yellow-500 mt-0.5" />
                              )}
                              <div className="flex-1">
                                <p className="text-sm font-medium capitalize">
                                  {error.type}
                                </p>
                                <p className="text-sm text-gray-600">
                                  {error.message}
                                </p>
                                {error.line && (
                                  <p className="text-xs text-gray-500 mt-1">
                                    Line {error.line}
                                  </p>
                                )}
                              </div>
                            </div>
                          </CardContent>
                        </Card>
                      ))}
                    </div>
                  )}
                </div>
              </TabsContent>
            </div>
          </Tabs>
        </div>
      </div>

      <div className="bg-gray-100 border-t border-gray-200 px-4 py-2 text-xs text-gray-600">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-4">
            <span>L Programming Language v1.0.0</span>
            <span>•</span>
            <span>{isEvaluating ? "Evaluating..." : "Ready"}</span>
          </div>
          <div className="flex items-center gap-4">
            <span>UTF-8</span>
            <span>•</span>
            <span>LF</span>
            <span>•</span>
            <span>Ln {code.split("\n").length}, Col 1</span>
          </div>
        </div>
      </div>
    </div>
  );
}
