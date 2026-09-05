'use client';

import {
  ArticleDocument,
  DeleteArticleDocument,
  UpsertArticleDocument,
} from '@/graphql/Articles';
import { useActions } from '@/lib/actions';
import { ErrorPage } from '@/ui/ErrorPage';
import { ActionGroup } from '@/ui/ActionGroup';
import { TitleBar } from '@/ui/TitleBar';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useRouter } from 'next/navigation';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useController, useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { CheckboxElement } from '../fields/checkbox';
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
  isVisible: z.boolean().prefault(true),
  attachmentIds: z.array(z.string()).prefault([]),
});

type FormValues = z.infer<typeof Form>;

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
        isVisible: data?.isVisible ?? true,
        attachmentIds:
          data?.explicitAttachments.nodes
            .map((attachment) => attachment.file?.id)
            .filter(isTruthy) ?? [],
      },
      {
        keepDirtyValues: true,
        keepTouched: true,
        keepErrors: true,
      },
    );
  }, [data, reset]);

  const onSubmit = async (values: FormValues) => {
    const result = await upsert({
      input: {
        info: {
          id: id || undefined,
          title: values.atJmeno,
          preview: values.atPreview,
          body: values.atText,
          titlePhotoUrl: values.titlePhotoUrl,
          isVisible: values.isVisible,
        },
        attachments: values.attachmentIds,
      },
    });
    if (!result.error && !id) {
      const newId = result.data?.upsertArticle?.aktuality?.id;
      toast.success('Přidáno.');
      if (newId) {
        router.replace(`/aktuality/${newId}`);
      } else {
        reset();
      }
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
      <TitleBar title={title}>
        <ActionGroup actions={actions} />
      </TitleBar>

      <FormError error={result.error} />
      <TextFieldElement control={control} name="atJmeno" label="Název" required />
      <ImageUrlField control={control} name="titlePhotoUrl" label="Titulní fotka" />
      <CheckboxElement control={control} name="isVisible" value="1" label="Veřejný" />

      <RichTextEditor
        control={control}
        initialState={data?.atPreview}
        name="atPreview"
        label="Shrnutí"
      />
      <RichTextEditor
        control={control}
        initialState={data?.atText}
        name="atText"
        label="Text"
      />
      <FilePicker value={attachmentIds.value ?? []} onChange={attachmentIds.onChange} />
      <SubmitButton control={control} />
    </form>
  );
}
