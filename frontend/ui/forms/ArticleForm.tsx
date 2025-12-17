import {
    ArticleDocument,
    CreateArticleDocument,
    DeleteArticleDocument,
    UpdateArticleDocument,
} from '@/graphql/Articles';
import { useConfirm } from '@/ui/Confirm';
import { ErrorPage } from '@/ui/ErrorPage';
import { TitleBar } from '@/ui/TitleBar';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useRouter } from 'next/router';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  atJmeno: z.string().min(1, 'Zadejte název článku'),
  atPreview: z.string().optional().prefault(''),
  atText: z.string().optional().prefault(''),
  titlePhotoUrl: z.preprocess(
    (val) => val === "" ? null : val,
    z.string().nullable().default(null),
  ),
});

type FormValues = z.infer<typeof Form>;

export function ArticleForm({ id = '' }: { id?: string }) {
  const router = useRouter();
  const confirm = useConfirm();
  const [query] = useQuery({ query: ArticleDocument, variables: { id }, pause: !id });
  const data = query.data?.aktuality;
  const title = id ? data?.atJmeno || '(Bez názvu)' : 'Nový článek';

  const create = useMutation(CreateArticleDocument)[1];
  const update = useMutation(UpdateArticleDocument)[1];
  const deleteMutation = useMutation(DeleteArticleDocument)[1];

  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    reset({
      atJmeno: data?.atJmeno ?? '',
      atPreview: data?.atPreview ?? '',
      atText: data?.atText ?? '',
      titlePhotoUrl: data?.titlePhotoUrl ?? '',
    }, {
      keepDirtyValues: true,
      keepTouched: true,
      keepErrors: true,
    });
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (patch: FormValues) => {

    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createAktuality?.aktuality?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace({
          pathname: '/aktuality/[id]',
          query: { id },
        });
      } else {
        reset();
      }
    }
  });

  const remove = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete smazat příspěvek "${data?.atJmeno}"?` });
    await deleteMutation({ id })
    router.replace('/aktuality')
  }, [router, confirm, deleteMutation, id, data?.atJmeno]);

  if (query.data && query.data.aktuality === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar title={title}>
        {data && (
          <DropdownMenu>
            <DropdownMenuTrigger.CornerDots />
            <DropdownMenuContent align="end">
              <DropdownMenuButton onClick={remove}>
                Smazat
              </DropdownMenuButton>
            </DropdownMenuContent>
          </DropdownMenu>
        )}
      </TitleBar>

      <FormError error={onSubmit.error} />
      <TextFieldElement control={control} name="atJmeno" label="Název" required />
      <TextFieldElement control={control} name="titlePhotoUrl" label="URL hlavní fotky" />

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
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}
