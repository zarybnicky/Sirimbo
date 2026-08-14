'use client';

import {
  ArticleDocument,
  CreateArticleDocument,
  DeleteArticleDocument,
  UpdateArticleDocument,
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
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { CheckboxElement } from '../fields/checkbox';

const Form = z.object({
  atJmeno: z.string().min(1, 'Zadejte název článku'),
  atPreview: z.string().optional().prefault(''),
  atText: z.string().optional().prefault(''),
  titlePhotoUrl: z.preprocess(
    (val) => (val === '' ? null : val),
    z.string().nullable().default(null),
  ),
  isVisible: z.boolean().prefault(true),
});

type FormValues = z.infer<typeof Form>;

export function ArticleForm({ id = '' }: { id?: string }) {
  const router = useRouter();
  const [query] = useQuery({ query: ArticleDocument, variables: { id }, pause: !id });
  const data = query.data?.aktuality;
  const title = id ? data?.atJmeno || '(Bez názvu)' : 'Nový článek';

  const [createResult, create] = useMutation(CreateArticleDocument);
  const [updateResult, update] = useMutation(UpdateArticleDocument);

  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    reset(
      {
        atJmeno: data?.atJmeno ?? '',
        atPreview: data?.atPreview ?? '',
        atText: data?.atText ?? '',
        titlePhotoUrl: data?.titlePhotoUrl ?? '',
        isVisible: data?.isVisible ?? true,
      },
      {
        keepDirtyValues: true,
        keepTouched: true,
        keepErrors: true,
      },
    );
  }, [data, reset]);

  const onSubmit = async (patch: FormValues) => {
    if (id) {
      await update({ id, patch });
    } else {
      const result = await create({ input: patch });
      if (!result.error) {
        const id = result.data?.createAktuality?.aktuality?.id;
        toast.success('Přidáno.');
        if (id) {
          router.replace(`/aktuality/${id}`);
        } else {
          reset();
        }
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
          await router.replace('/aktuality');
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

      <FormError error={createResult.error || updateResult.error} />
      <TextFieldElement control={control} name="atJmeno" label="Název" required />
      <TextFieldElement control={control} name="titlePhotoUrl" label="URL hlavní fotky" />
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
      <SubmitButton control={control} />
    </form>
  );
}
