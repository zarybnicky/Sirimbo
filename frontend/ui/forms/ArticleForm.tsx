'use client';

import {
  ArticleDocument,
  DeleteArticleDocument,
  UpsertArticleDocument,
} from '@/graphql/Articles';
import { useActions } from '@/lib/actions';
import { ErrorPage } from '@/ui/ErrorPage';
import { PageHeader } from '@/ui/PageHeader';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { DatePickerElement } from '@/ui/fields/date';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useRouter } from 'next/navigation';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useController, useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { FilePicker, ImageUrlField } from '@/ui/forms/FilePicker';
import { isTruthy } from '@/lib/truthyFilter';

const Form = z.object({
  atJmeno: z.string().min(1, 'Zadejte název článku'),
  atPreview: z.string().optional().prefault(''),
  atText: z.string().optional().prefault(''),
  titlePhotoUrl: z.preprocess(
    (val) => (val === '' ? null : val),
    z.string().nullable().default(null),
  ),
  isVisible: z.boolean().prefault(false),
  publishedAt: z.date().nullable().prefault(null),
  attachmentIds: z.array(z.string()).prefault([]),
});

export function ArticleForm({ id = '' }: { id?: string }) {
  const router = useRouter();
  const [query] = useQuery({ query: ArticleDocument, variables: { id }, pause: !id });
  const data = query.data?.aktuality;
  const title = id ? data?.atJmeno || '(Bez názvu)' : 'Nový článek';

  const [result, upsert] = useMutation(UpsertArticleDocument);

  const { reset, control, handleSubmit } = useForm({
    defaultValues: { attachmentIds: [] },
    resolver: zodResolver(Form),
  });
  const { field: attachmentIds } = useController({ control, name: 'attachmentIds' });
  React.useEffect(() => {
    reset(
      {
        atJmeno: data?.atJmeno ?? '',
        atPreview: data?.atPreview ?? '',
        atText: data?.atText ?? '',
        titlePhotoUrl: data?.titlePhotoUrl ?? '',
        isVisible: data?.isVisible ?? false,
        publishedAt: data?.publishedAt ? new Date(data.publishedAt) : null,
        attachmentIds:
          data?.explicitAttachments.nodes.map((x) => x.file?.id).filter(isTruthy) ?? [],
      },
      {
        keepDirtyValues: true,
        keepTouched: true,
        keepErrors: true,
      },
    );
  }, [data, reset]);

  const onSubmit: Parameters<typeof handleSubmit>[0] = async (values, event) => {
    const intent = (event?.nativeEvent as SubmitEvent | undefined)?.submitter?.getAttribute(
      'value',
    );
    const isVisible =
      intent === 'publish' ? true : intent === 'unpublish' ? false : values.isVisible;
    const result = await upsert({
      input: {
        info: {
          id: id || undefined,
          title: values.atJmeno,
          preview: values.atPreview,
          body: values.atText,
          titlePhotoUrl: values.titlePhotoUrl,
          isVisible,
          publishedAt: values.publishedAt?.toISOString() ?? null,
        },
        attachments: values.attachmentIds,
      },
    });
    if (result.error) return;
    toast.success(
      intent === 'publish'
        ? 'Článek byl zveřejněn.'
        : intent === 'unpublish'
          ? 'Článek byl schován.'
          : 'Uloženo.',
    );
    if (!id) {
      const newId = result.data?.upsertArticle?.result?.id;
      if (newId) router.replace(`/aktuality/${newId}`);
    }
  };
  const actions = useActions(
    [
      {
        id: 'article.delete',
        label: 'Smazat',
        visible: () => !!data && !!id,
        confirm: ({ item }: { item: { id: string; title: string } }) => ({
          description: `Opravdu chcete smazat příspěvek "${item.title}"?`,
        }),
        execute: async ({ item, mutate, router }) => {
          await mutate(DeleteArticleDocument, { id: item.id });
          router.replace('/aktuality');
        },
      },
    ],
    data && id ? { id, title: data.atJmeno } : null,
  );

  if (query.data && query.data.aktuality === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <PageHeader
        title={title}
        breadcrumbLabel={id ? 'Upravit článek' : undefined}
        breadcrumbs={[{ label: 'Aktuality', href: '/aktuality' }]}
        actions={actions}
      />

      <FormError error={result.error} />
      <TextFieldElement control={control} name="atJmeno" label="Název" required />
      <ImageUrlField control={control} name="titlePhotoUrl" label="Titulní fotka" />
      <DatePickerElement control={control} name="publishedAt" label="Datum prvního zveřejnění" />

      <RichTextEditor
        control={control}
        name="atPreview"
        label="Shrnutí"
        enableImageUpload
      />
      <RichTextEditor
        control={control}
        name="atText"
        label="Text"
        enableImageUpload
      />
      <FilePicker value={attachmentIds.value ?? []} onChange={attachmentIds.onChange} />
      <div className="flex flex-wrap gap-2">
        <SubmitButton
          control={control}
          name="intent"
          value="save"
          disabled={!!id && !data}
        >
          {data?.isVisible ? 'Uložit změny' : 'Uložit koncept'}
        </SubmitButton>

        <SubmitButton
          control={control}
          name="intent"
          value={data?.isVisible ? 'unpublish' : 'publish'}
          variant="outline"
          disabled={!!id && !data}
        >
          {data?.isVisible ? 'Schovat' : 'Zveřejnit'}
        </SubmitButton>
      </div>
    </form>
  );
}
