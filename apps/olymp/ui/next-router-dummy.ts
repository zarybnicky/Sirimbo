export function useRouter() {
    return {
        asPath: typeof window !== 'undefined' ? window.location.pathname : '',
        query: {},
    };
}
