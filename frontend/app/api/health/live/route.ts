export function GET() {
  return Response.json({ ok: true }, {
    headers: { 'Cache-Control': 'private, no-store, max-age=0, must-revalidate' },
  });
}
