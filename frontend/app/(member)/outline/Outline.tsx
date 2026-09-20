'use client';

import { OutlineBreadcrumbDocument } from '@/graphql/Document';
import { OutlinePane } from '@/ui/OutlinePane';
import { PageHeader } from '@/ui/TitleBar';
import { nodeText } from '@/ui/fields/outline/blocks';
import Link from 'next/link';
import { useQuery } from 'urql';

export function Outline({ root }: { root?: string }) {
  return (
    <div className="col-feature">
      <PageHeader title="Outline" />
      {root && <Breadcrumb node={root} />}
      <OutlinePane root={root} editable />
    </div>
  );
}

function Breadcrumb({ node }: { node: string }) {
  const [{ data }] = useQuery({
    query: OutlineBreadcrumbDocument,
    variables: { node },
  });

  return (
    <nav className="mb-2 flex flex-wrap items-center gap-1 text-sm text-neutral-11">
      <Link href="/outline" className="underline">
        Vše
      </Link>
      {(data?.documentNodePathList ?? []).map((ancestor) => (
        <span key={ancestor.id} className="flex items-center gap-1">
          <span aria-hidden>›</span>
          <Link href={`/outline/${ancestor.id}`} className="underline">
            {nodeText(ancestor.content) || 'Bez názvu'}
          </Link>
        </span>
      ))}
    </nav>
  );
}
