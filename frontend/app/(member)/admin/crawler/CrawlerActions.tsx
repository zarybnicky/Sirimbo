import { Download, RotateCcw } from 'lucide-react';
import { toast } from 'react-toastify';
import { ActionGroup } from '@/ui/ActionGroup';
import { queueCrawlerAction } from './actions';

export function CrawlerActions({
  federation,
  kind,
  keyContains = '',
  failureGroup,
  primary = false,
  onQueued,
}: {
  federation: string;
  kind: string;
  keyContains?: string;
  failureGroup?: Parameters<typeof queueCrawlerAction>[1]['failureGroup'];
  primary?: boolean;
  onQueued: () => void;
}) {
  async function queue(action: Parameters<typeof queueCrawlerAction>[0]) {
    try {
      const count = await queueCrawlerAction(action, {
        federation,
        kind,
        keyContains,
        failureGroup,
      });
      toast.success(`Queued ${count} targets`);
      onQueued();
    } catch (error) {
      toast.error(error instanceof Error ? error.message : 'Could not queue targets.');
    }
  }

  return (
    <ActionGroup
      className="font-sans"
      variant={failureGroup || primary ? 'toolbar' : 'row'}
      primary={failureGroup || primary ? ['refetch-errors', 'reprocess-all'] : undefined}
      iconOnly={!!failureGroup || primary}
      align="end"
      actions={[
        {
          id: 'refetch-errors',
          key: 'refetch-errors',
          label: failureGroup ? 'Refetch this failure group' : 'Refetch errors',
          icon: Download,
          variant: primary ? 'primary' : 'outline',
          execute: () => queue('refetch-errors'),
        },
        {
          id: 'reprocess-all',
          key: 'reprocess-all',
          label: failureGroup ? 'Reprocess this failure group' : 'Reprocess all',
          icon: RotateCcw,
          variant: primary ? 'primary' : 'outline',
          execute: () => queue('reprocess-all'),
        },
      ]}
    />
  );
}
