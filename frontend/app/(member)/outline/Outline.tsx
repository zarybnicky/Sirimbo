'use client';

import { OutlineBreadcrumbDocument } from '@/graphql/Document';
import { PageHeader } from '@/ui/TitleBar';
import { OutlineBreadcrumb } from '@/ui/outline/OutlineBreadcrumb';
import { OutlinePane } from '@/ui/outline/OutlinePane';
import { useQuery } from 'urql';

export function Outline({ root }: { root?: string }) {
  return (
    <div className="col-feature">
      <PageHeader title="Outline" />
      {root && <Zoom node={root} />}
      <OutlinePane root={root} editable />
    </div>
  );
}

// The ancestors of the node being zoomed to; the node itself is what the pane
// below is showing.
function Zoom({ node }: { node: string }) {
  const [{ data }] = useQuery({
    query: OutlineBreadcrumbDocument,
    variables: { node },
  });

  return (
    <OutlineBreadcrumb
      ancestors={data?.documentNodePathList ?? []}
      className="mb-2 text-sm"
    />
  );
}
