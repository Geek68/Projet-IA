document.addEventListener('DOMContentLoaded', () => {
    const crimeSelect = document.getElementById('crime-type');
    const suspectSelect = document.getElementById('suspect');
    const investigateBtn = document.getElementById('investigate-btn');
    const resultsContainer = document.getElementById('results-content');
    const historyBody = document.getElementById('history-body');

    const evidenceText = {
        motive: 'Le suspect avait un mobile.',
        near_scene: 'Le suspect était près de la scène de crime.',
        fingerprint: 'Empreintes du suspect sur l\'arme du crime.',
        eyewitness: 'Un témoin oculaire a identifié le suspect.',
        dna: 'ADN du suspect trouvé sur les lieux.',
        camera: 'Le suspect a été vu sur une caméra de surveillance.',
        bank: 'Transaction bancaire suspecte enregistrée.',
        fake_id: 'Le suspect possède une fausse identité.'
    };

    investigateBtn.addEventListener('click', async () => {
        const crime = crimeSelect.value;
        const suspect = suspectSelect.value;

        try {
            const response = await fetch('http://localhost:8000/investigate', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({ crime, suspect })
            });

            if (!response.ok) {
                throw new Error(`Erreur HTTP: ${response.status}`);
            }

            const data = await response.json();
            
            displayResults(data.verdict, data.suspicionLevel, data.evidence);
            addToHistory(suspect, crime, data.verdict ? 'Coupable' : 'Non coupable');

        } catch (error) {
            console.error('Erreur lors de la requête:', error);
            resultsContainer.innerHTML = `<p class="text-red-500">Impossible de contacter le serveur Prolog. Assurez-vous qu'il est bien lancé.</p>`;
        }
    });

    function displayResults(isGuilty, level, evidenceList) {
        const verdictText = isGuilty ? 'Coupable' : 'Non coupable';
        const verdictColor = isGuilty ? 'text-red-500' : 'text-green-500';

        let evidenceHtml = '<ul class="text-left mt-4 list-disc list-inside">';
        if (evidenceList.length > 0) {
            evidenceList.forEach(itemKey => {
                const text = evidenceText[itemKey] || 'Preuve non reconnue.';
                evidenceHtml += `<li>${text}</li>`;
            });
        } else {
            evidenceHtml += '<li>Aucune preuve trouvée.</li>';
        }
        evidenceHtml += '</ul>';

        resultsContainer.innerHTML = `
            <h3 class="text-2xl font-bold ${verdictColor}">${verdictText}</h3>
            <p class="text-gray-300 mt-2">Niveau de suspicion</p>
            <div class="w-full bg-gray-700 rounded-full h-4 mt-2">
                <div class="bg-blue-600 h-4 rounded-full" style="width: ${level}%;"></div>
            </div>
            <p class="text-sm font-bold text-white mt-1">${level}%</p>
            <div class="mt-6">
                <h4 class="text-xl font-semibold text-white">Preuves</h4>
                ${evidenceHtml}
            </div>
        `;
    }

    function addToHistory(suspect, crime, verdict) {
        const newRow = document.createElement('tr');
        newRow.className = 'bg-gray-800 border-b border-gray-700';
        newRow.innerHTML = `
            <td class="p-3">${suspect.charAt(0).toUpperCase() + suspect.slice(1)}</td>
            <td class="p-3">${crime.charAt(0).toUpperCase() + crime.slice(1)}</td>
            <td class="p-3 ${verdict === 'Coupable' ? 'text-red-500' : 'text-green-500'} font-semibold">${verdict}</td>
        `;
        historyBody.prepend(newRow);
    }
});