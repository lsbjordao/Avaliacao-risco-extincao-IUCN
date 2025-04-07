
  // Aguardar o carregamento completo do DOM antes de adicionar o ouvinte de evento
  document.addEventListener("DOMContentLoaded", function() {

    // Adiciona o ouvinte de evento de tecla
    document.addEventListener("keydown", function(event) {

      // Se a seta para a direita foi pressionada
      if (event.key === "ArrowRight") {
        // Buscar o elemento de navegação para a direita (próxima página)
        const nextPageButton = document.querySelector(".nav-page-next a");

        // Verificar se o elemento foi encontrado e simula o clique
        if (nextPageButton) {
          nextPageButton.click();
        }
      }

      // Se a seta para a esquerda foi pressionada
      if (event.key === "ArrowLeft") {
        // Buscar o elemento de navegação para a esquerda (página anterior)
        const prevPageButton = document.querySelector(".nav-page-previous a");

        // Verificar se o elemento foi encontrado e simula o clique
        if (prevPageButton) {
          prevPageButton.click();
        }
      }
    });
  });
