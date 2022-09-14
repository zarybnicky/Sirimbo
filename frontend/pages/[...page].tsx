import * as React from 'react';
import { ReactPage } from '../components/ReactPage';
import { Container, Typography } from '@mui/material';
import { useRouter } from 'next/router';
import { useTypedQuery, $ } from 'lib/query';

export const DynamicPage = () => {
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
    {json.title && json.title !== 'TK Olymp' && (
      <div className="header-section">
        <div className="container full">
          <h1>{json.title}</h1>
          {json.subheader}
        </div>
      </div>
    )}

    {json.messages.map((msg: any, i: number) => (
      <div className="container" key={i}>
        <div className="alert alert-{{ msg.type }}" dangerouslySetInnerHTML={{ __html: msg.text }} />
      </div>
    ))}

    <Typography variant="h3" component="h2">{json.title}</Typography>

    <div dangerouslySetInnerHTML={{ __html: json.content }} />
  </Container>;
}

export default DynamicPage;
