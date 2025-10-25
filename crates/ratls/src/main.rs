use anyhow::Result;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() -> Result<()> {
    let (service, socket) = LspService::new(|_client| ratls_server::Backend::default());
    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket)
        .serve(service)
        .await;
    Ok(())
}

mod ratls_server {
    use tower_lsp::lsp_types::*;
    use tower_lsp::{jsonrpc::Result, LanguageServer};

    #[derive(Default)]
    pub struct Backend;

    #[tower_lsp::async_trait]
    impl LanguageServer for Backend {
        async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
            Ok(InitializeResult::default())
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
    }
}
