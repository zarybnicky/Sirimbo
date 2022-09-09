import * as React from 'react';
import { gql, useQuery } from '@apollo/client';
import { ReactPage } from '../components/ReactPage';
import { Container, Typography } from '@mui/material';
import { useRouter } from 'next/router';

const GET_PAGE = gql(`
query GetPage($url: String!) {
  pageByUrl(url: $url) {
    content
  }
}`);

export const DynamicPage = () => {
  const router = useRouter();
  const { data } = useQuery(GET_PAGE, { variables: { url: router.pathname } });
  return <ReactPage readOnly value={data?.pageByUrl?.content} />;
}

export const OldDynamicPage = () => {
  const router = useRouter();
  const [json, setJson] = React.useState<any>({});;
  React.useEffect(() => {
    (async () => {
      const res = await fetch(`${router.pathname}?no-layout=1`);
      const json = await res.json();
      setJson(json);
    })();
  }, []);
  return <Container maxWidth="lg" style={{ marginTop: 80 }}>
    <Typography variant="h3" component="h2">{json.title}</Typography>

    <div dangerouslySetInnerHTML={{ __html: json.content }} />
  </Container>;
}

export default DynamicPage;
