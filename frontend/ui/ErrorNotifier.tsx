import React from 'react';
import { captureException } from '@sentry/nextjs';
import { toast } from 'react-toastify';
import { TypedEventTarget } from 'typescript-event-target';
import type { CombinedError } from 'urql';
import { getErrorMessage } from '@/lib/errors';
import { getNewDeploymentId } from '@/lib/build-id';

export const errorTarget = new TypedEventTarget<{ error: CustomEvent<CombinedError> }>();
let deploymentRecovery: Promise<boolean> | undefined;

const onError = async ({ detail: combined }: CustomEvent<CombinedError>) => {
  if (combined.networkError) {
    toast.error(
      `Nastal problém s připojením k serveru (${combined.networkError.message})`,
    );
    return;
  }

  if (
    combined.graphQLErrors.length > 0 &&
    combined.graphQLErrors.every(
      (ex) => !ex.path && ex.locations?.length && !getErrorMessage(ex.message),
    )
  ) {
    try {
      deploymentRecovery ??= getNewDeploymentId()
        .then((deploymentId) => {
          if (!deploymentId) return false;
          const key = `graphql-reload:${deploymentId}`;
          if (window.sessionStorage.getItem(key)) return false;
          window.sessionStorage.setItem(key, '1');
          window.location.reload();
          return true;
        })
        .finally(() => { deploymentRecovery = undefined; });
      if (await deploymentRecovery) return;
    } catch {}
  }

  for (const ex of combined.graphQLErrors) {
    const message = getErrorMessage(ex.message);
    toast.error(message ?? ex.message);
    if (!message) {
      captureException(combined);
    }
  }
};

export function ErrorNotifier() {
  React.useEffect(() => {
    errorTarget.addEventListener('error', onError);
    return () => errorTarget.removeEventListener('error', onError);
  }, []);
  return null;
}
