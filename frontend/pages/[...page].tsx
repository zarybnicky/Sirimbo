import * as React from 'react';
import { ReactPage } from '../components/ReactPage';
import { useRouter } from 'next/router';
import { useTypedQuery, fetchTypedQuery } from 'lib/query';
import { $ } from 'lib/zeus';
import { GetServerSideProps } from 'next';

const DynamicPage: React.FC<{
  content: object;
}> = () => {
  const router = useRouter();
  const { data } = useTypedQuery(['page', router.pathname], {
    pageByUrl: [
      { url: $('url', 'String!') },
      { content: true }
    ],
  }, {}, {
    variables: { url: router.pathname },
  });
  return <ReactPage readOnly value={data?.pageByUrl?.content} />;
}

export default DynamicPage;

export const getServerSideProps: GetServerSideProps = async (context) => {
  const { resolvedUrl } = context;

  const { pageByUrl } = await fetchTypedQuery({
    pageByUrl: [
      { url: $('url', 'String!') },
      { content: true }
    ],
  }, {
    variables: { url: resolvedUrl },
  });

  if (!pageByUrl) {
    return { notFound: true };
  }

  return {
    props: {
      content: pageByUrl.content,
    },
  };
};
