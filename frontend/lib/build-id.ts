export const buildId = process.env.NEXT_DEPLOYMENT_ID || 'develop';

export async function getNewDeploymentId(): Promise<string | undefined> {
  if (process.env.NODE_ENV !== 'production' || typeof window === 'undefined') return;

  const currentDeploymentId = document.documentElement.dataset.dplId ?? buildId;
  if (!currentDeploymentId || currentDeploymentId === 'develop') return;

  const response = await fetch('/api/health', {
    cache: 'no-store',
    headers: { Accept: 'application/json' },
  });
  if (!response.ok) return;

  const { deploymentId } = (await response.json()) as { deploymentId?: string | null };
  if (!deploymentId || deploymentId === 'develop') return;

  if (deploymentId !== currentDeploymentId) {
    return deploymentId;
  }
}
