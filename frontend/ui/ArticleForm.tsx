import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { AktualityInput } from '@app/graphql';
import {
  ArticleDocument,
  CreateArticleDocument,
  DeleteArticleDocument,
  UpdateArticleDocument,
} from '@app/graphql/Articles';
import { useMutation, useQuery } from 'urql';
import { useRouter } from 'next/router';
import { toast } from 'react-toastify';
import { ErrorPage } from './ErrorPage';
import { RichTextEditor } from '@app/ui/fields/richtext';
import { TitleBar } from './TitleBar';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTriggerDots } from './dropdown';
import { useConfirm } from './Confirm';

type FormProps = Pick<AktualityInput, 'atJmeno' | 'atPreview' | 'atText'>;

export const ArticleForm = ({ id = '' }: { id?: string }) => {
  const router = useRouter();
  const confirm = useConfirm();
  const [query] = useQuery({ query: ArticleDocument, variables: { id }, pause: !id });
  const data = query.data?.aktuality;
  const title = id ? data?.atJmeno || '(Bez názvu)' : 'Nový článek';

  const create = useMutation(CreateArticleDocument)[1];
  const update = useMutation(UpdateArticleDocument)[1];
  const deleteMutation = useMutation(DeleteArticleDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      atJmeno: data?.atJmeno,
      atPreview: data?.atPreview,
      atText: data?.atText,
    });
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createAktuality?.aktuality?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace(`/aktuality/${id}`);
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.aktuality === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar title={title}>
        {data && (
          <DropdownMenu>
            <DropdownMenuTriggerDots />
            <DropdownMenuContent align="end">
              <DropdownMenuButton
                onClick={async () => {
                  await confirm({ description: `Opravdu chcete smazat příspěvek "${data.atJmeno}"?` });
                  await deleteMutation({ id })
                  router.replace('/aktuality')
                }}
              >
                Smazat
              </DropdownMenuButton>
            </DropdownMenuContent>
          </DropdownMenu>
        )}
      </TitleBar>

      <FormError error={onSubmit.error} />
      <TextFieldElement control={control} name="atJmeno" label="Název" required />
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
};
