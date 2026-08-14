'use client';

import { AnnouncementDocument } from '@/graphql/Announcement';
import { useQuery } from 'urql';
import { AnnouncementMeta, useAnnouncementActions } from '@/ui/AnnouncementShared';
import { PageHeader } from '@/ui/TitleBar';
import React from 'react';
import { AnnouncementForm } from '@/ui/forms/AnnouncementForm';
import { RichTextView } from '@/ui/RichTextView';

export function Announcement({ id }: { id: string }) {
  const [editing, setEditing] = React.useState(false);
  const [query] = useQuery({
    query: AnnouncementDocument,
    variables: { id },
    pause: !id,
  });
  const data = query.data?.announcement;
  const loading = !id || query.fetching;
  const startEditing = React.useCallback(() => setEditing(true), []);
  const stopEditing = React.useCallback(() => setEditing(false), []);
  const actions = useAnnouncementActions(data, startEditing);
  const pageTitle = loading
    ? 'Načítám příspěvek…'
    : data?.title || 'Příspěvek nebyl nalezen';

  return (
    <div className="py-4 lg:py-8">
      <PageHeader
        title={pageTitle}
        breadcrumbs={[{ label: 'Nástěnka', href: '/nastenka' }, { label: pageTitle }]}
        actions={data ? actions : undefined}
        subtitle={data ? <AnnouncementMeta item={data} /> : undefined}
      />
      {loading ? (
        <p className="text-neutral-11 text-center">Příspěvek se právě načítá.</p>
      ) : !data ? (
        <p className="text-neutral-11 text-center">
          Příspěvek je nedostupný nebo už neexistuje.
        </p>
      ) : editing ? (
        <AnnouncementForm id={data.id} data={data} onSuccess={stopEditing} />
      ) : (
        <RichTextView className="max-w-none" value={data.body} />
      )}
    </div>
  );
}
