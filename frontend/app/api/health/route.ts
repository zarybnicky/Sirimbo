type DeploymentResponse = {
  ok: boolean;
  deploymentId: string | null;
};

// eslint-disable-next-line import-x/no-unused-modules
export function GET() {
  return Response.json(
    {
      ok: true,
      deploymentId: process.env.NEXT_DEPLOYMENT_ID || null,
    } satisfies DeploymentResponse,
    {
      headers: {
        'Cache-Control': 'private, no-store, max-age=0, must-revalidate',
      },
    },
  );
}
