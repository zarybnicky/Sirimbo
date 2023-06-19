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
import { DeleteButton } from './DeleteButton';
import { Route } from 'nextjs-routes';
import { RichTextEditor } from './RichTextEditor';
import { TitleBar } from './layout/TitleBar';

type FormProps = Pick<AktualityInput, 'atJmeno' | 'atPreview' | 'atText'>;

const backHref: Route = { pathname: '/admin/aktuality' };

export const ArticleForm = ({ id = '' }: { id?: string }) => {
  const router = useRouter();
  const [query] = useQuery({ query: ArticleDocument, variables: { id } });
  const data = query.data?.aktuality;
  const title = id ? data?.atJmeno || '(Bez názvu)' : 'Nový článek';

  const create = useMutation(CreateArticleDocument)[1];
  const update = useMutation(UpdateArticleDocument)[1];

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
      await create({ input: patch });
      const res = await create({ input: patch });
      const id = res.data?.createAktuality?.aktuality?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace({ pathname: '/admin/aktuality/[id]', query: { id } });
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
      <TitleBar backHref={backHref} title={title}>
        {id && (
          <DeleteButton
            doc={DeleteArticleDocument}
            id={id}
            title="smazat článek"
            redirect={backHref}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
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
    </form>
  );
};
