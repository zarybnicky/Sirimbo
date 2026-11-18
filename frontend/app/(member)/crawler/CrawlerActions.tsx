'use client';

import { Ban, Download, RotateCcw } from 'lucide-react';
import { toast } from 'react-toastify';
import type { CrawlerTargetQuery } from '@/graphql/Crawler';
import type { ResolvedAction } from '@/lib/actions';
import { ActionGroup } from '@/ui/ActionGroup';
import { queueCrawlerAction } from './actions';

export function CrawlerActions({
  federation,
  kind,
  keyContains = '',
  failureGroup,
  frontier,
  primary = false,
  onQueued,
}: {
  federation: string;
  kind: string;
  keyContains?: string;
  failureGroup?: Parameters<typeof queueCrawlerAction>[1]['failureGroup'];
  frontier?: Pick<
    NonNullable<CrawlerTargetQuery['crawlerTarget']>,
    'id' | 'fetchStatus' | 'lastSuccessfulResponseId'
  >;
  primary?: boolean;
  onQueued?: () => void;
}) {
  async function queue(action: Parameters<typeof queueCrawlerAction>[0]) {
    try {
      const count = await queueCrawlerAction(action, {
        federation,
        kind,
        keyContains,
        failureGroup,
        frontierId: frontier?.id,
      });
      toast.success(
        action === 'mark-gone' ? 'Marked as gone.' : `Queued ${count} targets`,
      );
      onQueued?.();
    } catch (error) {
      toast.error(error instanceof Error ? error.message : 'Could not queue targets.');
    }
  }

  const actions: ResolvedAction[] = [
    {
      id: 'refetch',
      key: 'refetch',
      label: frontier
        ? 'Refetch now'
        : failureGroup
          ? 'Refetch this failure group'
          : 'Refetch errors',
      icon: Download,
      variant: primary ? 'primary' : 'outline',
      execute: () => queue('refetch'),
    },
  ];
  if (!frontier || (frontier.fetchStatus === 'OK' && frontier.lastSuccessfulResponseId)) {
    actions.push({
      id: 'reprocess',
      key: 'reprocess',
      label: frontier
        ? 'Reprocess stored response'
        : failureGroup
          ? 'Reprocess this failure group'
          : 'Reprocess all',
      icon: RotateCcw,
      variant: primary ? 'primary' : 'outline',
      execute: () => queue('reprocess'),
    });
  }
  if (frontier && frontier.fetchStatus !== 'GONE') {
    actions.push({
      id: 'mark-gone',
      key: 'mark-gone',
      label: 'Mark as gone',
      icon: Ban,
      variant: 'danger',
      confirm:
        'Stop fetching this frontier? Stored responses will be kept. Rediscovery or “Refetch now” can revive it.',
      execute: () => queue('mark-gone'),
    });
  }

  return (
    <ActionGroup
      className="font-sans"
      variant={failureGroup || primary ? 'toolbar' : 'row'}
      primary={failureGroup || primary ? ['refetch', 'reprocess'] : undefined}
      iconOnly={!!failureGroup || primary}
      align="end"
      actions={actions}
    />
  );
}
