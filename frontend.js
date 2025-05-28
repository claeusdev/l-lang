// In frontend.js
function evaluateCode() {
	const codeInput = document.getElementById('code').value;
	const outputResultArea = document.getElementById('outputResult');
	const outputASTArea = document.getElementById('outputAST');
	const outputEnvArea = document.getElementById('outputEnv');

	// Clear previous outputs and provide feedback
	outputResultArea.textContent = 'Evaluating...';
	outputASTArea.textContent = '';
	outputEnvArea.textContent = '';

	fetch('/evaluate', {
		method: 'POST',
		headers: {
			'Content-Type': 'text/plain',
		},
		body: codeInput
	})
		.then(response => {
			if (!response.ok) {
				throw new Error(`HTTP error! status: ${response.status}`);
			}
			return response.json();
		})
		.then(data => {
			outputASTArea.textContent = data.parsedAST || 'N/A'; // Display AST
			if (data.evaluationError) {
				outputResultArea.textContent = `Error: ${data.evaluationError}`;
			} else {
				outputResultArea.textContent = data.evaluationResult || 'No result (e.g., only definition)';
			}
			// Display Environment (pretty-printed JSON)
			outputEnvArea.textContent = JSON.stringify(data.currentEnvironment, null, 2) || 'N/A';
		})
		.catch(error => {
			outputResultArea.textContent = `Workspace Error: ${error.message}`;
			console.error('Error:', error);
		});
}
