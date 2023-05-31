import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { AktualityInput } from 'lib/graphql';
import {ArticleDocument, CreateArticleDocument, DeleteArticleDocument, UpdateArticleDocument} from 'lib/graphql/Articles';
import dynamic from 'next/dynamic';
import { useMutation, useQuery } from 'urql';
import { useRouter } from 'next/router';
import { toast } from 'react-toastify';
import { ErrorPage } from './ErrorPage';
import { DeleteButton } from './DeleteButton';
import { Item } from './layout/Item';
import { Route } from 'nextjs-routes';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });

type FormProps = Pick<AktualityInput, 'atJmeno' | 'atPreview' | 'atText'>;

const backHref: Route = { pathname: '/admin/aktuality' };

export const ArticleForm = ({ id = '' }: { id?: string }) => {
  const router = useRouter();
  const [query] = useQuery({ query: ArticleDocument, variables: { id } });
  const data = query.data?.aktuality;

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
    if (data) {
      await update({ id: data.id, patch });
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
      <Item.Titlebar
        backHref={backHref}
        title={id ? data?.atJmeno || '(Bez názvu)' : 'Nový článek'}
      >
        {id && (
          <DeleteButton
            doc={DeleteArticleDocument}
            id={id}
            title="smazat článek"
            onDelete={() => router.push(backHref)}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </Item.Titlebar>

      <ErrorBox error={onSubmit.error} />
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
