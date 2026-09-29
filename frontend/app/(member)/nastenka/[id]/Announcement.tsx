'use client';

import { AnnouncementDocument } from '@/graphql/Announcement';
import { useQuery } from 'urql';
import { AnnouncementMeta } from '@/ui/AnnouncementShared';
import { PageHeader } from '@/ui/TitleBar';
import { AnnouncementForm } from '@/ui/forms/AnnouncementForm';
import { RichTextView } from '@/ui/RichTextView';
import { FileAttachments } from '@/ui/FileAttachments';
import { announcementActions } from '@/lib/actions/announcement';
import { useActions } from '@/lib/actions';
import { useRouter, useSearchParams } from 'next/navigation';
import { FormResultContext } from '@/ui/form';

export function Announcement({ id }: { id: string }) {
  const router = useRouter();
  const searchParams = useSearchParams();
  const [query] = useQuery({
    query: AnnouncementDocument,
    variables: { id },
    pause: !id,
  });
  const data = query.data?.announcement;
  const loading = !id || query.fetching;
  const actions = useActions(announcementActions, data);
  const editing =
    searchParams?.get('edit') === '1' &&
    actions.some((action) => action.id === 'announcement.edit');
  const pageTitle = loading
    ? 'Načítám příspěvek…'
    : data?.title || 'Příspěvek nebyl nalezen';
  const exitEditing = () => router.replace(`/nastenka/${id}`);

  return (
    <>
      <PageHeader
        title={pageTitle}
        breadcrumbs={[{ label: 'Nástěnka', href: '/nastenka' }, { label: pageTitle }]}
        primary="announcement.edit"
        actions={
          data
            ? actions.filter((action) => !editing || action.id !== 'announcement.edit')
            : undefined
        }
        subtitle={data ? <AnnouncementMeta item={data} /> : undefined}
      />
      {loading ? (
        <p className="text-neutral-11 text-center">Příspěvek se právě načítá.</p>
      ) : !data ? (
        <p className="text-neutral-11 text-center">
          Příspěvek je nedostupný nebo už neexistuje.
        </p>
      ) : editing ? (
        <FormResultContext.Provider
          value={{ onSuccess: exitEditing, onCancel: exitEditing }}
        >
          <AnnouncementForm id={data.id} data={data} />
        </FormResultContext.Provider>
      ) : (
        <>
          <RichTextView className="max-w-none" value={data.body} />
          <FileAttachments attachments={data.explicitAttachments.nodes} />
        </>
      )}
    </>
  );
}
