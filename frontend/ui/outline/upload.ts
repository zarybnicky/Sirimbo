// POST /f stores the body under a new file row and answers with the row's own
// generated URL, which is what BlockNote puts in the block's props — and what the
// document_node_file trigger reads back out to link node to file.
export async function uploadOutlineFile(file: File): Promise<string> {
  const response = await fetch('/f', {
    method: 'POST',
    headers: {
      'content-type': file.type || 'application/octet-stream',
      'x-file-name': encodeURIComponent(file.name),
    },
    body: file,
  });

  const body = (await response.json()) as { url?: string; error?: string };
  if (!response.ok || !body.url) {
    throw new Error(body.error || `Soubor ${file.name} se nepodařilo nahrát.`);
  }
  return body.url;
}
